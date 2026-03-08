import java.lang.reflect.*;
import java.util.*;
import java.util.stream.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class five {

    interface CharacterProducer {
        char produce();
    }

    @FunctionalInterface
    interface EnterpriseCharacterFactory<T extends CharacterProducer> {
        T create(int asciiOffset);
    }

    static class BaseCharacterProducer implements CharacterProducer {
        private final int asciiCode;
        BaseCharacterProducer(int asciiCode) { this.asciiCode = asciiCode; }
        @Override public char produce() { return (char) asciiCode; }
    }

    interface StringAssemblyStrategy {
        String assemble(List<CharacterProducer> producers);
    }

    static class SequentialAssemblyStrategy implements StringAssemblyStrategy {
        @Override
        public String assemble(List<CharacterProducer> producers) {
            return producers.stream()
                .map(p -> String.valueOf(p.produce()))
                .collect(Collectors.joining());
        }
    }

    interface StringObserver {
        void onCharacterAdded(char c, int position);
        void onComplete(String result);
    }

    static class LoggingObserver implements StringObserver {
        private final List<String> log = new ArrayList<>();
        @Override public void onCharacterAdded(char c, int position) {
            log.add("Position " + position + ": '" + c + "' added (ASCII: " + (int)c + ")");
        }
        @Override public void onComplete(String result) {
        }
    }

    static class ObservableCharacterProducer implements CharacterProducer {
        private final CharacterProducer wrapped;
        private final List<StringObserver> observers;
        private final int position;

        ObservableCharacterProducer(CharacterProducer wrapped, List<StringObserver> observers, int position) {
            this.wrapped = wrapped;
            this.observers = observers;
            this.position = position;
        }

        @Override
        public char produce() {
            char c = wrapped.produce();
            observers.forEach(o -> o.onCharacterAdded(c, position));
            return c;
        }
    }

    static class CachedCharacterProducer implements CharacterProducer {
        private final CharacterProducer wrapped;
        private Character cache = null;

        CachedCharacterProducer(CharacterProducer wrapped) { this.wrapped = wrapped; }

        @Override
        public char produce() {
            if (cache == null) cache = wrapped.produce();
            return cache;
        }
    }

    static class ThreadSafeCharacterProducer implements CharacterProducer {
        private final CharacterProducer wrapped;
        private final AtomicReference<Character> cache = new AtomicReference<>();

        ThreadSafeCharacterProducer(CharacterProducer wrapped) { this.wrapped = wrapped; }

        @Override
        public synchronized char produce() {
            cache.compareAndSet(null, wrapped.produce());
            return cache.get();
        }
    }

    static class EnterpriseStringBuilder {
        private final List<CharacterProducer> producers = new ArrayList<>();
        private StringAssemblyStrategy strategy = new SequentalAssemblyStrategy();
        private final List<StringObserver> observers = new ArrayList<>();

        public EnterpriseStringBuilder withStrategy(StringAssemblyStrategy strategy) {
            this.strategy = strategy;
            return this;
        }

        public EnterpriseStringBuilder addObserver(StringObserver observer) {
            observers.add(observer);
            return this;
        }

        public EnterpriseStringBuilder addCharacter(int ascii) {
            CharacterProducer base = new BaseCharacterProducer(ascii);
            CharacterProducer cached = new CachedCharacterProducer(base);
            CharacterProducer safe = new ThreadSafeCharacterProducer(cached);
            CharacterProducer observed = new ObservableCharacterProducer(safe, observers, producers.size());
            producers.add(observed);
            return this;
        }

        public String build() {
            String result = strategy.assemble(producers);
            observers.forEach(o -> o.onComplete(result));
            return result;
        }
    }

    static class SecretMessageVault {
        private static final String ENCODED = Base64.getEncoder()
            .encodeToString("9EGLINSTER".getBytes(StandardCharsets.UTF_8));

        public static int[] getAsciiCodes() {
            byte[] decoded = Base64.getDecoder().decode(ENCODED);
            int[] codes = new int[decoded.length];
            for (int i = 0; i < decoded.length; i++) codes[i] = decoded[i];
            return codes;
        }
    }

    static class OutputRegistry {
        private static volatile OutputRegistry instance;
        private final Map<String, String> registry = new ConcurrentHashMap<>();

        private OutputRegistry() {}

        public static OutputRegistry getInstance() {
            if (instance == null) {
                synchronized (OutputRegistry.class) {
                    if (instance == null) instance = new OutputRegistry();
                }
            }
            return instance;
        }

        public void register(String key, String value) { registry.put(key, value); }
        public String retrieve(String key) { return registry.get(key); }
    }

    interface PrintCommand {
        void execute();
    }

    static class ConsolePrintCommand implements PrintCommand {
        private final String message;
        ConsolePrintCommand(String message) { this.message = message; }

        @Override
        public void execute() {
            System.out.println(message);
        }
    }

    static class CommandQueue {
        private final Queue<PrintCommand> queue = new LinkedList<>();

        public void enqueue(PrintCommand cmd) { queue.add(cmd); }

        public void drainAndExecute() {
            while (!queue.isEmpty()) queue.poll().execute();
        }
    }

    static class ReflectiveOrchestrator {
        public static void orchestrate() throws Exception {
            Method method = five.class.getDeclaredMethod("buildAndRegisterString");
            method.setAccessible(true);
            method.invoke(null);
        }
    }

    static void buildAndRegisterString() {
        int[] codes = SecretMessageVault.getAsciiCodes();

        EnterpriseStringBuilder builder = new EnterpriseStringBuilder()
            .withStrategy(new SequentialAssemblyStrategy())
            .addObserver(new LoggingObserver());

        for (int code : codes) {
            builder.addCharacter(code);
        }

        String result = builder.build();
        OutputRegistry.getInstance().register("OUTPUT", result);
    }

    static class EnterpriseExecutionEngine {
        public static void run() throws Exception {
            ForkJoinPool pool = new ForkJoinPool(1);
            pool.submit(() -> {
                try {
                    String output = OutputRegistry.getInstance().retrieve("OUTPUT");
                    CommandQueue queue = new CommandQueue();
                    queue.enqueue(new ConsolePrintCommand(output));
                    queue.drainAndExecute();
                } catch (Exception e) {
                    throw new RuntimeException("Catastrophic output failure", e);
                }
            }).get();
            pool.shutdown();
        }
    }

    public static void main(String[] args) throws Exception {
        ReflectiveOrchestrator.orchestrate();
        EnterpriseExecutionEngine.run();
    }
}