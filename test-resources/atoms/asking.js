// SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
// SPDX-License-Identifier: MIT

// The λ function 'L_number_plus', answered by a program that reduces nothing
// itself. An operand arrives as it was written, so 'Φ.number( … ).plus( … )'
// reaches this script whole, and getting the number out of it is dataization,
// which is phino's business and not a script's. So the script asks, naming
// rather than quoting: a line of its own with an 'id' it minted, the 'of' of
// the request being served and the 'attr' of the operand it wants, with
// 'reduce' asking phino to dataize it. Such a question is served from the
// receiver phino already holds for that request, in lean text without the ρ
// chain (#1165), and answered with that 'id', the node under '𝑛' and its byte
// array under 'Δ' (#1206) — where quoting the receiver in an 'ask' used to
// make the next question twice as big as the last.

// Every request is a coroutine, so a question suspends the request that asked
// it rather than the script: serving a question fires atoms of its own, and one
// of them may well be this very atom, whose request arrives while the question
// is still open.

'use strict';

const readline = require('readline');

// The bytes phino answered a question with, which the answer spells under 'Δ'
// whenever the node it carries is a byte formation, so this script reads no 𝜑
// at all (#1206).
function bytes(answer) {
  return 'Δ' in answer ? Buffer.from(answer['Δ'].replace(/-/g, ''), 'hex') : Buffer.alloc(0);
}

// The double a byte array carries, or no number at all when it is not eight
// octets long.
function number(answer) {
  const raw = bytes(answer);
  return raw.length === 8 ? raw.readDoubleBE(0) : NaN;
}

// A double as the byte array 𝜑-calculus spells it.
function hex(value) {
  const raw = Buffer.alloc(8);
  raw.writeDoubleBE(value, 0);
  return [...raw].map((octet) => octet.toString(16).toUpperCase().padStart(2, '0')).join('-');
}

// The λ function itself: every 𝜑-expression it yields is a question, and what
// comes back is the answer phino reduced, so both operands arrive as bytes
// however deeply they were written. The operands are named by reference: 'of'
// is the request being served, 'attr' the attribute of its receiver, and
// 'reduce' says phino should dataize the value rather than hand the node over.
function* plus(request) {
  const rho = number(yield { of: request, attr: 'ρ', reduce: true });
  const x = number(yield { of: request, attr: 'x', reduce: true });
  return Number.isNaN(rho) || Number.isNaN(x)
    ? '⊥'
    : `Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ ${hex(rho + x)} ⟧ ) )`;
}

// The questions phino has not answered yet, by the 'id' each was minted with,
// every one of them holding the request it suspended.
const open = new Map();

let minted = 0;

function said(message) {
  process.stdout.write(`${JSON.stringify(message)}\n`);
}

// Run one request on, with what phino answered its last question: either the
// next question, or the answer to the request once there are no more.
function advance(atom, id, answer) {
  const step = atom.next(answer);
  if (step.done) {
    said({ id, '𝑛': step.value });
    return;
  }
  minted += 1;
  open.set(minted, { atom, id });
  said({ id: minted, ...step.value });
}

readline.createInterface({ input: process.stdin }).on('line', (line) => {
  const message = JSON.parse(line);
  if ('𝑏' in message) {
    advance(plus(message.id), message.id, undefined);
  } else if (open.has(message.id)) {
    const waiting = open.get(message.id);
    open.delete(message.id);
    advance(waiting.atom, waiting.id, message);
  }
});
