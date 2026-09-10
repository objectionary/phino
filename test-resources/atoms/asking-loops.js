// SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
// SPDX-License-Identifier: MIT

// The λ function 'L_number_gt', answered by a resident program that asks
// phino to reduce a term the universe can never finish reducing:
// 'Φ.nan.gt( 1 )' walks dispatches into a cycle that ends only when the
// step budget dies. Under '--partial' phino parks where the term cycles and
// answers the question with the residual (#1078), so the run lives on;
// without the flag the exhausted budget fails it, exactly as before the ask
// channel existed (#1160). This script answers every request with the bytes
// of 42 whatever the question came back with: a run that ends on '2A-' is
// the proof that a looping question did not take the dataization down.

'use strict';

const readline = require('readline');

// The questions phino has not answered yet, by the 'id' each was minted
// with, every one of them holding the request it suspended.
const open = new Map();

let minted = 0;

function said(message) {
  process.stdout.write(`${JSON.stringify(message)}\n`);
}

readline.createInterface({ input: process.stdin }).on('line', (line) => {
  const message = JSON.parse(line);
  if ('λ' in message) {
    minted += 1;
    open.set(minted, message.id);
    said({ id: minted, ask: 'Φ.nan.gt( 1 )' });
  } else if ('𝑛' in message && open.has(message.id)) {
    const id = open.get(message.id);
    open.delete(message.id);
    said({ id, '𝑛': '⟦ Δ ⤍ 2A- ⟧' });
  }
});
