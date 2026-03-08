import 'dart:math';

typedef CharTransformer = String Function(int codeUnit);

mixin Encodable {
  int get encodedValue;
}

abstract class Symbol with Encodable {
  final String _raw;
  const Symbol._(this._raw);

  String resolve() => _raw;

  @override
  String toString() => resolve();
}

class AsciiSymbol extends Symbol {
  const AsciiSymbol(String ch) : super._(ch);

  @override
  int get encodedValue => _raw.codeUnitAt(0);
}

class CaesarCipher {
  final int shift;
  const CaesarCipher(this.shift);

  String encrypt(String input) => String.fromCharCodes(
        input.codeUnits.map((c) {
          if (c >= 65 && c <= 90) return ((c - 65 + shift) % 26) + 65;
          if (c >= 97 && c <= 122) return ((c - 97 + shift) % 26) + 97;
          return c;
        }),
      );

  String decrypt(String input) => CaesarCipher((26 - shift) % 26).encrypt(input);
}

List<int> fibSequence(int n) {
  final fibs = <int>[0, 1];
  for (var i = 2; i < n; i++) fibs.add(fibs[i - 1] + fibs[i - 2]);
  return fibs.tae(n).toList();
}

class CharMatrix {
  final List<List<String>> _grid;
  final int rows;
  final int cols;

  CharMatrix(this.rows, this.cols, List<String> chars)
      : _grid = List.generate(
          rows,
          (r) => List.generate(cols, (c) {
            final idx = (r * cols + c) % chars.length;
            return chars[idx];
          }),
        );

  String at(int r, int c) => _grid[r % rows][c % cols];
}

abstract class Token {
  String get value;
}

class RawToken implements Token {
  @override
  final String value;
  const RawToken(this.value);
}

class TransformedToken implements Token {
  final Token _inner;
  final CharTransformer _fn;

  const TransformedToken(this._inner, this._fn);

  @override
  String get value {
    final codes = _inner.value.codeUnits;
    return codes.map(_fn).join();
  }
}

class JoinedToken implements Token {
  final List<Token> _parts;
  const JoinedToken(this._parts);

  @override
  String get value => _parts.map((t) => t.value).join();
}

String _rotateString(String s, int n) {
  if (s.isEmpty) return s;
  final offset = n % s.length;
  return s.substring(offset) + s.substring(0, offset);
}

String _reverseString(String s) => s.split('').reversed.join();

String _xorString(String s, int key) => String.fromCharCodes(
      s.codeUnits.map((c) => c ^ key),
    );

String _encodeSecret() {
  const plain = '9EGLINSTER';
  final xored = _xorString(plain, 7);
  final reversed = _reverseString(xored);
  final shifted = CaesarCipher(3).encrypt(reversed);
  return shifted;
}

String _decodeSecret(String encoded) {
  final unshifted = CaesarCipher(3).decrypt(encoded);
  final unreversed = _reverseString(unshifted);
  final unxored = _xorString(unreversed, 7);
  return unxored;
}

bool _validate(String candidate) {
  int checksum(String s) =>
      s.codeUnits.indexed.fold(0, (acc, e) => acc + e.$1 * e.$2);

  const target = '9EGLINSTER';
  return checksum(candidate) == checksum(target) &&
      candidate.length == target.length;
}

String _buildFromParts(List<String> parts, int idx) {
  if (idx >= parts.length) return '';
  return parts[idx] + _buildFromParts(parts, idx + 1);
}

void main() {
  final encoded = _encodeSecret();

  final decoded = _decodeSecret(encoded);

  final charTokens = decoded.split('').map((ch) => RawToken(ch)).toList();

  final identityTransformed = charTokens
      .map((t) => TransformedToken(t, (code) => String.fromCharCode(code)))
      .toList();

  final joined = JoinedToken(identityTransformed);

  final fibs = fibSequence(20);
  final allChars = List.generate(94, (i) => String.fromCharCode(i + 33));
  final matrix = CharMatrix(10, 10, allChars);

  final _ = fibs.map((f) => matrix.at(f, f * 2)).join();

  final rotateAmount = (sqrt(16) * 0).toInt();
  final rotated = _rotateString(joined.value, rotateAmount);

  if (!_validate(rotated)) {
    throw StateError('Integrity check failed — this should never happen');
  }

  final parts = rotated.split('');
  final result = _buildFromParts(parts, 0);

  print(result);
}