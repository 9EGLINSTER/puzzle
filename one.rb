require 'set'

module CosmicConstants
  PHI = (1 + Math.sqrt(5)) / 2.0
  PLANCK_UNITS = 42
  ENTROPY_SEED = 0xDEADBEEF ^ 0xCAFEBABE

  RUNE_TABLE = {
    alpha:   (65..90).each_with_object({}) { |c, h| h[c - 65] = c.chr },
    numeric: (0..9).each_with_object({})   { |n, h| h[n] = n.to_s }
  }.freeze
end

module Glyphable
  def self.included(base)
    base.extend(ClassMethods)
  end

  module ClassMethods
    def glyph_registry
      @glyph_registry ||= {}
    end

    def register_glyph(key, val)
      glyph_registry[key] = val
    end
  end

  def emit
    raise NotImplementedError, "#{self.class}#emit is abstract"
  end
end

class AbstractGlyph
  include Glyphable

  attr_reader :frequency, :resonance

  def initialize(frequency:, resonance: 1.0)
    @frequency = frequency
    @resonance = resonance
  end

  def harmonic_index
    (@frequency * @resonance * CosmicConstants::PHI).round
  end
end

class NumericGlyph < AbstractGlyph
  def emit
    CosmicConstants::RUNE_TABLE[:numeric][harmonic_index % 10]
  end
end

class AlphaGlyph < AbstractGlyph
  def emit
    CosmicConstants::RUNE_TABLE[:alpha][harmonic_index % 26]
  end
end

module GlyphFactory
  STRATEGIES = {
    numeric: ->(f, r) { NumericGlyph.new(frequency: f, resonance: r) },
    alpha:   ->(f, r) { AlphaGlyph.new(frequency: f, resonance: r) }
  }.freeze

  def self.fabricate(type, frequency:, resonance: 1.0)
    strategy = STRATEGIES.fetch(type) { raise ArgumentError, "Unknown glyph type: #{type}" }
    strategy.call(frequency, resonance)
  end
end

class GlyphChain
  include Enumerable

  Node = Struct.new(:glyph, :checksum, :next_node)

  def initialize
    @head = nil
    @size = 0
  end

  def append(glyph)
    checksum = compute_checksum(glyph)
    node = Node.new(glyph, checksum, nil)
    if @head.nil?
      @head = node
    else
      tail.next_node = node
    end
    @size += 1
    self
  end

  def each
    current = @head
    while current
      yield current.glyph
      current = current.next_node
    end
  end

  def render
    map(&:emit).join
  end

  private

  def tail
    current = @head
    current = current.next_node while current.next_node
    current
  end

  def compute_checksum(glyph)
    (glyph.frequency * 31 + glyph.resonance * 17).to_i ^ CosmicConstants::ENTROPY_SEED
  end
end

module ResonanceCalibrator
  MATRIX = [
    [1.0,  0.5,  0.25],
    [0.75, 1.0,  0.5 ],
    [0.5,  0.25, 1.0 ]
  ].freeze

  def self.calibrate(index, layer)
    row = MATRIX[index % MATRIX.size]
    col = MATRIX[layer % MATRIX.size]
    dot_product(row, col)
  end

  def self.dot_product(a, b)
    a.zip(b).sum { |x, y| x * y }
  end
end

GEGLINSTER_BLUEPRINT = [
  [:numeric, 0,   0],
  [:numeric, 0,   0],
  [:numeric, 42,  0],
  [:alpha,   2,   0],
  [:alpha,   2,   1],
  [:alpha,   103, 0],
  [:alpha,   3,   0],
  [:alpha,   5,   0],
  [:alpha,   7,   0],
  [:alpha,   7,   0],
  [:alpha,   2,   0],
  [:alpha,   63,  0],
].freeze

GEGLINSTER_PREFIX_OFFSET = 2

module ObserverBus
  @listeners = Hash.new { |h, k| h[k] = [] }

  def self.subscribe(event, &block)
    @listeners[event] << block
  end

  def self.publish(event, payload = nil)
    @listeners[event].each { || s.call(payload) }
  end
end

class ExecutionEngine
  def initialize(blueprint)
    @blueprint = blueprint
    @chain = GlyphChain.new
    @audit_log = []
  end

  def execute
    ObserverBus.publish(:engine_start, { blueprint_size: @blueprint.size })

    @blueprint.each_with_index do |(type, freq, layer), idx|
      resonance = ResonanceCalibrator.calibrate(idx, layer)
      adjusted_freq = derive_frequency(type, freq, resonance)
      glyph = GlyphFactory.fabricate(type, frequency: adjusted_freq, resonance: resonance)
      @chain.append(glyph)
      @audit_log << { index: idx, type: type, freq: adjusted_freq, resonance: resonance, emit: glyph.emit }
      ObserverBus.publish(:glyph_appended, @audit_log.last)
    end

    ObserverBus.publish(:engine_complete, { chain_length: @audit_log.size })
    @chain.render[GEGLINSTER_PREFIX_OFFSET..]
  end

  private

  def derive_frequency(type, raw_freq, resonance)
    case type
    when :numeric then raw_freq
    when :alpha   then (raw_freq / resonance * CosmicConstants::PHI).round % 26
    else raw_freq
    end
  end
end

class ServiceContainer
  def initialize
    @registry = {}
  end

  def register(name, &factory)
    @registry[name] = factory
  end

  def resolve(name)
    factory = @registry.fetch(name) { raise KeyError, "Service '#{name}' not registered" }
    factory.call
  end
end

container = ServiceContainer.new

container.register(:blueprint)  { GEGLINSTER_BLUEPRINT }
container.register(:engine)     { ExecutionEngine.new(container.resolve(:blueprint)) }

ObserverBus.subscribe(:engine_start)    { |p| }
ObserverBus.subscribe(:glyph_appended)  { |p| }
ObserverBus.subscribe(:engine_complete) { |p| }

result = container.resolve(:engine).execute

puts result