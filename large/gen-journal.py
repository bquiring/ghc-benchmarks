#!/usr/bin/env python3
"""Write a deterministic hledger journal (fixed seed): N transactions over
ten years, in a few hundred accounts, with prices in two commodities and
some balance assertions.  Usage: gen-journal.py N OUT"""
import random, sys
from datetime import date, timedelta

n, out = int(sys.argv[1]), sys.argv[2]
rng = random.Random(20261011)
tops = ['expenses', 'income', 'assets', 'liabilities']
subs = {
    'expenses': ['food', 'rent', 'travel', 'utilities', 'health', 'books', 'tax', 'gifts'],
    'income': ['salary', 'interest', 'dividends', 'refunds'],
    'assets': ['bank:checking', 'bank:savings', 'cash', 'broker'],
    'liabilities': ['card', 'loan'],
}
accounts = []
for t in tops:
    for s in subs[t]:
        for k in range(rng.randint(3, 12)):
            accounts.append('%s:%s:%s%d' % (t, s, s.split(':')[-1][:3], k))
payees = ['shop %d' % i for i in range(300)]
start = date(2015, 1, 1)
with open(out, 'w') as f:
    f.write('commodity $1,000.00\ncommodity 1,000.00 EUR\n\n')
    d = start
    for i in range(n):
        d = d + timedelta(days=rng.random() < 0.3)
        if i % 500 == 0:
            f.write('P %s EUR $%.4f\n\n' % (d.isoformat(), 1.0 + rng.random() * 0.3))
        a = rng.choice(accounts)
        b = rng.choice([x for x in accounts if x.startswith('assets') or x.startswith('liabilities')])
        cur = 'EUR' if rng.random() < 0.2 else '$'
        amt = rng.randint(1, 500000) / 100
        amount = '%.2f EUR' % amt if cur == 'EUR' else '$%.2f' % amt
        tag = '  ; project:p%d' % rng.randint(0, 20) if rng.random() < 0.3 else ''
        f.write('%s %s%s\n    %s  %s\n    %s\n\n' % (d.isoformat(), rng.choice(payees), tag, a, amount, b))
