#!/usr/bin/env python3

import struct
import sys

program = []
labels = {}
counter = 0

with open(sys.argv[1], 'rt') as f:
    for line in f:
        line = line.strip()
        if not line: continue
        if line.startswith('#'):
            continue
        if line.endswith(':'):
            labels[line.rstrip(':')] = counter
            continue
        if line.startswith("'"):
            c = bytes(line.strip("'"), 'utf-8').decode('unicode_escape')
            program.append(str(ord(c)))
            counter += 2
            continue
        if line.startswith('"'):
            program.append('0')
            counter += 2
            for c in reversed(bytes(line.strip('"'), 'utf-8').
                              decode('unicode_escape')):
                program.append(str(ord(c)))
                counter += 2
            continue
        program.append(line)
        counter += 1
        if line.isdigit() or line.startswith('-'):
            value = int(line)
            if -2**7 <= value < 2**7:
                counter += 1
            elif -2**15 <= value < 2**15:
                counter += 2
            elif -2**31 <= value < 2**31:
                counter += 4
            else:
                print >> sys.stderr, "constant too large:", value
                sys.exit(1)
        elif line.startswith('jump') or line.startswith('jnz'):
            counter += 2
        elif line.startswith('dup') or line.startswith('swap'):
            counter += 1

opcode = {
  'halt': b'\x00',
  'jump': b'\x01',
  'jnz': b'\x02',
  'dup': b'\x03',
  'swap': b'\x04',
  'drop': b'\x05',
  'push4': b'\x06',
  'push2': b'\x07',
  'push1': b'\x08',
  'add': b'\x09',
  'sub': b'\x0a',
  'mul': b'\x0b',
  'div': b'\x0c',
  'mod': b'\x0d',
  'eq': b'\x0e',
  'ne': b'\x0f',
  'lt': b'\x10',
  'gt': b'\x11',
  'le': b'\x12',
  'ge': b'\x13',
  'not': b'\x14',
  'and': b'\x15',
  'or': b'\x16',
  'input': b'\x17',
  'output': b'\x18',
  'clock': b'\x2a',
  'cons': b'\x30',
  'hd': b'\x31',
  'tl': b'\x32',
}

byteorder = 'little'

with open(sys.argv[2], 'wb') as f:
    for instr in program:
        if instr.isdigit() or instr.startswith('-'):
            value = int(instr)
            if -2**7 <= value < 2**7:
                f.write(opcode['push1'])
                f.write(value.to_bytes(1, byteorder=byteorder, signed=True))
            elif -2**15 <= value < 2**15:
                f.write(opcode['push2'])
                f.write(value.to_bytes(2, byteorder=byteorder, signed=True))
            elif -2**31 <= value < 2**31:
                f.write(opcode['push4'])
                f.write(value.to_bytes(4, byteorder=byteorder, signed=True))
            else:
                print >> sys.stderr, "constant too large:", value
                sys.exit(1)
        elif instr.startswith('jump') or instr.startswith("jnz"):
            words = instr.split()
            f.write(opcode[words[0]])
            f.write(labels[words[1]].to_bytes(2, byteorder=byteorder,
                                              signed=False))
        elif instr.startswith('dup') or instr.startswith('swap'):
            words = instr.split()
            f.write(opcode[words[0]])
            f.write(int(words[1]).to_bytes(1, byteorder=byteorder,
                                           signed=False))
        else:
            f.write(opcode[instr])
