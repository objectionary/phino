// SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
// SPDX-License-Identifier: MIT

// The λ functions phino's own tests fire. phino implements none of them: it
// writes this script to a temporary file, runs it under 'node' and talks to it
// over stdin and stdout, one JSON object per line, in the letters of the
// evaluation rule of the calculus: the universe Φ under '𝑒', then a request
// with an 'id', the name of the λ function under 'λ' and the formation being
// evaluated under '𝑏', which this script answers with a line carrying the same
// 'id' and, under '𝑛', the 𝜑-expression to use as the atom's raw result. The
// lines are read as they come and answered as they come, so the script serves
// just as well started once per fire as kept for the run with 'serve'.
//
// This is a fixture, not a runtime: it goes just far enough to let the specs
// reduce arithmetic and byte comparisons. Both payloads arrive as canonical
// 𝜑-calculus, so every datum spells its bytes out as a Δ binding and the first
// one inside an operand is the operand's own. An operand that is not an
// already-reduced datum is not dataized here: a real runtime would ask phino
// for it, with 'phino dataize --inside=...'.

'use strict';

const readline = require('readline');

const OPENING = '⟦(';
const CLOSING = '⟧)';

// The 𝜑 text bound to the attribute 'name' by the formation 'b'.
function bound(b, name) {
  const head = name + ' ↦ ';
  let depth = 0;
  for (let index = 0; index < b.length; index += 1) {
    const char = b[index];
    if (OPENING.includes(char)) {
      depth += 1;
    } else if (CLOSING.includes(char)) {
      depth -= 1;
    } else if (depth === 1 && b.startsWith(head, index) && ' ,⟦'.includes(b[index - 1])) {
      return bindingValue(b, index + head.length);
    }
  }
  return null;
}

// The 𝜑 text of one binding's value: everything up to the comma or the closing
// bracket that ends it.
function bindingValue(b, start) {
  let depth = 0;
  let text = '';
  for (let index = start; index < b.length; index += 1) {
    const char = b[index];
    if (OPENING.includes(char)) {
      depth += 1;
    } else if (CLOSING.includes(char)) {
      if (depth === 0) {
        break;
      }
      depth -= 1;
    } else if (char === ',' && depth === 0) {
      break;
    }
    text += char;
  }
  return text.trim();
}

// Every shape an already-reduced datum reaches this script in: a literal
// argument, a copy of the 'number', 'string' or 'bytes' object bound to ρ, or a
// bare byte formation. The bytes are what follows. Since #1142 and #1155 both
// bindings of a datum are named φ (the only void the real 'bytes', 'number'
// and 'string' declare), so the φ shapes come first; the legacy 'as-bytes'
// and 'data' shapes stay for inputs jeo wrote.
const DATA = [
  'Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ ',
  'Φ.string( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ ',
  'Φ.bytes( φ ↦ ⟦ Δ ⤍ ',
  '⟦ φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ ',
  '⟦ φ ↦ ⟦ Δ ⤍ ',
  '⟦ Δ ⤍ ',
  'Φ.number( as-bytes ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ ',
  'Φ.string( as-bytes ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ ',
  '⟦ as-bytes ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ ',
  'Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ ',
  'Φ.string( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ ',
  'Φ.bytes( data ↦ ⟦ Δ ⤍ ',
  '⟦ as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ ',
  '⟦ data ↦ ⟦ Δ ⤍ ',
];

// The bytes of an already-reduced datum, or nothing at all when the operand is
// not one: this fixture never guesses at an operand that still has to be
// dataized, since dataizing it is a phino run of its own.
function bytes(expr) {
  const shape = expr === null ? undefined : DATA.find((prefix) => expr.startsWith(prefix));
  if (shape === undefined) {
    return null;
  }
  const found = /^([0-9A-F-]+)/.exec(expr.slice(shape.length));
  return found === null ? null : Buffer.from(found[1].replace(/-/g, ''), 'hex');
}

// The double a datum carries. A byte array of any other length carries no
// number at all, exactly as 'Expect.at(…).that(Number)' insists in EO.
function number(expr) {
  const raw = bytes(expr);
  return raw !== null && raw.length === 8 ? raw.readDoubleBE(0) : NaN;
}

// A byte array the way 𝜑-calculus spells one: '--' when it is empty, 'FF-'
// when it holds a single octet, '20-1F' when it holds more.
function hex(raw) {
  if (raw.length === 0) {
    return '--';
  }
  const octets = [...raw].map((octet) => octet.toString(16).toUpperCase().padStart(2, '0'));
  return octets.length === 1 ? `${octets[0]}-` : octets.join('-');
}

function asBytes(raw) {
  return `Φ.bytes( φ ↦ ⟦ Δ ⤍ ${hex(raw)} ⟧ )`;
}

function asNumber(value) {
  const raw = Buffer.alloc(8);
  raw.writeDoubleBE(value, 0);
  return `Φ.number( φ ↦ ${asBytes(raw)} )`;
}

function asBool(yes) {
  return yes ? 'Φ.true' : 'Φ.false';
}

// A number atom with an operand carrying no number yields the terminator ⊥,
// the way every EO number atom does.
function arithmetic(b, operation) {
  const left = number(bound(b, 'x'));
  const right = number(bound(b, 'ρ'));
  return Number.isNaN(left) || Number.isNaN(right) ? '⊥' : asNumber(operation(left, right));
}

// The 𝜑-expression the λ function 'atom' answers with for the formation 'b'.
function answer(atom, b) {
  switch (atom) {
    case 'L_number_plus':
      return arithmetic(b, (x, rho) => rho + x);
    case 'L_number_times':
      return arithmetic(b, (x, rho) => rho * x);
    case 'L_number_div':
      return arithmetic(b, (x, rho) => rho / x);
    case 'L_number_gt': {
      const left = number(bound(b, 'x'));
      const right = number(bound(b, 'ρ'));
      return Number.isNaN(left) || Number.isNaN(right) ? '⊥' : asBool(right > left);
    }
    case 'L_bytes_eq': {
      const left = bytes(bound(b, 'b'));
      const right = bytes(bound(b, 'ρ'));
      return left === null || right === null ? '⊥' : asBool(left.equals(right));
    }
    case 'L_bytes_not': {
      const raw = bytes(bound(b, 'ρ'));
      return raw === null ? '⊥' : asBytes(Buffer.from([...raw].map((octet) => ~octet & 0xff)));
    }
    default:
      throw new Error(`the fixture implements no atom named '${atom}'`);
  }
}

// The universe under '𝑒' is not needed here, since no operand is dataized, so
// its line is read and let go; every request is answered on the spot.
readline.createInterface({ input: process.stdin }).on('line', (line) => {
  const message = JSON.parse(line);
  if ('id' in message) {
    process.stdout.write(`${JSON.stringify({ id: message.id, '𝑛': answer(message['λ'], message['𝑏']) })}\n`);
  }
});
