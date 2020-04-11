#include <TaskGraph>

#include <iostream>
#include <algorithm>

using namespace tg;

#define CACHESIZE 256
#define CACHEMASK 0xff
#define CACHEHASH(pp) (pp & CACHEMASK)
// #define CHECKCACHE

#define NUM_REGS 32

typedef TaskGraph<int, int[NUM_REGS], int> interpreter_TaskGraph;

class CodeCache {
public:
	CodeCache() {
		for ( unsigned a = 0; a < CACHESIZE; ++a )
			cache[a].tag = -1;
	}

	inline void insert ( interpreter_TaskGraph *T, int pc ) {
		cache [ CACHEHASH(pc) ].data = T;
		cache [ CACHEHASH(pc) ].tag = pc;
	}

	inline bool check ( int pc ) {
		return cache[ CACHEHASH(pc) ].tag == pc;
	}

	inline interpreter_TaskGraph *get(int pc) {
#ifdef CHECKCACHE
		if (cache[CACHEHASH(pc)].tag != pc) {
			std::cout << "Error: unexpected code cache miss, pc=" << pc << "\n";
		}
#endif
	return cache[CACHEHASH(pc)].data;
	}
private:
	struct {
		interpreter_TaskGraph *data;
		int tag;
	} cache[CACHESIZE];
};


struct Instruction {
	unsigned opcode:5;
	unsigned rdst:5;
	union {
		struct {
			unsigned rsrc1:5;
			unsigned rsrc2:5;
			int shortimmediate:12;
		} s;
		int immediate:22;
	};
};

#define OP_NOP 0 /* do nothing */

#define OP_ADD 1 /* rdst := rsrc1 + rsrc2 */
#define OP_SUB 2 /* rdst := rsrc1 - rsrc2 */
#define OP_MUL 3 /* rdst := rsrc1 * rsrc2 */
#define OP_DIV 4 /* rdst := rsrc1 / rsrc2 */

#define OP_JMP 5 /* pc := immediate */
#define OP_BLT 6 /* pc := immediate if rdst < 0 */
#define OP_BLE 7 /* pc := immediate if rdst < 0 */

#define OP_LDIMM 8 /* rdst := immediate */
#define OP_LD    9 /* rdst := *(rsrc1 + shortimmediate) */
#define OP_ST   10 /* *(rdst + shortimmediate) := rsrc1 */

#define OP_INC  11 /* rdst := rdst + immediate */

#define OP_RET  31 /* return from interpreter; result is rdst */

Instruction op_add(unsigned rd, unsigned rs1, unsigned rs2) {
	Instruction i;
	i.opcode = OP_ADD; i.rdst = rd; i.s.rsrc1 = rs1; i.s.rsrc2 = rs2;
	return i;
}

Instruction op_sub(unsigned rd, unsigned rs1, unsigned rs2) {
	Instruction i;
	i.opcode = OP_SUB; i.rdst = rd; i.s.rsrc1 = rs1; i.s.rsrc2 = rs2;
	return i;
}

Instruction op_mul(unsigned rd, unsigned rs1, unsigned rs2) {
	Instruction i;
	i.opcode = OP_MUL; i.rdst = rd; i.s.rsrc1 = rs1; i.s.rsrc2 = rs2;
	return i;
}

Instruction op_div(unsigned rd, unsigned rs1, unsigned rs2) {
	Instruction i;
	i.opcode = OP_DIV; i.rdst = rd; i.s.rsrc1 = rs1; i.s.rsrc2 = rs2;
	return i;
}

Instruction op_jmp(int dst) {
	Instruction i;
	i.opcode = OP_JMP; i.immediate = dst;
	return i;
}

Instruction op_blt(unsigned rd, int dst) {
  Instruction i;
  i.opcode = OP_BLT; i.rdst = rd; i.immediate = dst;
  return i;
}

Instruction op_ble(unsigned rd, int dst) {
	Instruction i;
	i.opcode = OP_BLE; i.rdst = rd; i.immediate = dst;
	return i;
}

Instruction op_ldimm(unsigned rd, int v) {
	Instruction i;
	i.opcode = OP_LDIMM; i.rdst = rd; i.immediate = v;
	return i;
}

Instruction op_ld(unsigned rd, unsigned rs, int v) {
	Instruction i;
	i.opcode = OP_LD; i.rdst = rd; i.s.rsrc1 = rs; i.s.shortimmediate = v;
	return i;
}

Instruction op_inc(unsigned rd, int delta) {
	Instruction i;
	i.opcode = OP_INC; i.rdst = rd; i.immediate = delta;
	return i;
}

Instruction op_ret(unsigned r) {
	Instruction i;
	i.opcode = OP_RET; i.rdst = r;
	return i;
}

/*
 * build and return a bytecode array representing a function to compute
 * the factorial of register 0.
 *
 * res = 1;
 * for i=1 to N
 *  res = res*i
 * return res
 *
 */

Instruction* fac() {
	Instruction* code = new Instruction[100];
	int p = 0;
	const int RN = 0;   /* register 0 is input parameter N */
	const int Rres = 1; /* register 1 holds res */
	const int Ri = 2;   /* register 2 holds i */
	const int Rtmp = 3;
	
	code[p++] = op_ldimm(Rres, 1);
	code[p++] = op_ldimm(Ri, 1);
	int label = p;
	code[p++] = op_mul(Rres, Rres, Ri);
	code[p++] = op_inc(Ri, 1);
	/* test loop back condition i<N */
	code[p++] = op_sub(Rtmp, Ri, RN);
	code[p++] = op_ble(Rtmp, label);
	code[p++] = op_ret(Rres);
	
	return code;
}


int interpreter ( Instruction *instructions, int pc, int regs[] ) {
	while ( 1 ) {
		Instruction inst = instructions[pc];
		pc += 1;
		switch ( inst.opcode ) {
		case OP_NOP:
			break;
		case OP_ADD:
			regs[inst.rdst] = regs[inst.s.rsrc1] + regs[inst.s.rsrc2];
			break;
		case OP_SUB:
			regs[inst.rdst] = regs[inst.s.rsrc1] - regs[inst.s.rsrc2];
			break;
		case OP_MUL:
			regs[inst.rdst] = regs[inst.s.rsrc1] * regs[inst.s.rsrc2];
			break;
		case OP_DIV:
			regs[inst.rdst] = regs[inst.s.rsrc1] / regs[inst.s.rsrc2];
			break;
		case OP_JMP:
			pc = inst.immediate;
			break;
		case OP_BLT:
			if (regs[inst.rdst] < 0)
				pc = inst.immediate;
			break;
		case OP_BLE:
			if (regs[inst.rdst] <= 0)
				pc = inst.immediate;
			break;
		case OP_LDIMM:
			regs[inst.rdst] = inst.immediate;
			break;
		case OP_LD:
			regs[inst.rdst] = *reinterpret_cast<int*>(regs[inst.s.rsrc1] + inst.s.shortimmediate);
			break;
		case OP_ST:
			*reinterpret_cast<int*>(regs[inst.rdst] + inst.s.shortimmediate) = regs[inst.s.rsrc1];
			break;
		case OP_INC:
			regs[inst.rdst] += inst.immediate;
			break;
		case OP_RET:
			return regs[inst.rdst];
		}
	}
}

interpreter_TaskGraph *specialise_interpreter ( Instruction *instructions, int entrypc ) {
	int pc = entrypc;
	bool creating = true;
	interpreter_TaskGraph *T = new interpreter_TaskGraph();
	taskgraph(interpreter_TaskGraph, *T, tuple2(regs, newpc)) {
		tWhile ( 1 ) {
			while ( creating ) {
				Instruction inst = instructions[pc++];
				switch ( inst.opcode ) {
				case OP_ADD:
					regs[inst.rdst] = regs[inst.s.rsrc1] + regs[inst.s.rsrc2];
					break;
				case OP_SUB:
					regs[inst.rdst] = regs[inst.s.rsrc1] - regs[inst.s.rsrc2];
					break;
				case OP_MUL:
					regs[inst.rdst] = regs[inst.s.rsrc1] * regs[inst.s.rsrc2];
					break;
				case OP_DIV:
					regs[inst.rdst] = regs[inst.s.rsrc1] / regs[inst.s.rsrc2];
					break;
				case OP_BLT:
					tIf ( regs[inst.rdst] < 0 ) {
						if ( inst.immediate == entrypc ) {
							tContinue;
						} else {
							newpc = inst.immediate;
							tBreak;
						}
					}
					break;
				case OP_BLE:
					tIf ( regs[inst.rdst] <= 0 ) {
						if ( inst.immediate == entrypc ) {
							tContinue;
						} else {
							newpc = inst.immediate;
							tBreak;
						}
					}
					break;
				case OP_LDIMM:
					regs[inst.rdst] = inst.immediate;
					break;
				case OP_INC:
					regs[inst.rdst] += inst.immediate;
					break;
				case OP_RET:
					newpc = -1;
					tReturn(regs[inst.rdst]);
					creating = false;
					break;
				}
			}
			tReturn(0);
		}
	}
	return T;
}

static CodeCache cache;
static int newpc, result;

int interpreter_tg ( Instruction *instructions, int pc, int regs[] ) {
	newpc = pc;

	while ( newpc >= 0 ) {
		if (!cache.check ( newpc ) ) {
			interpreter_TaskGraph *T = specialise_interpreter ( instructions, newpc );
			T->compile ( tg::GCC, 1 );
			cache.insert ( T, newpc );
		}

		interpreter_TaskGraph *T = cache.get(newpc);
		result = T->execute ( regs, newpc );
	}
	return result;
}

unsigned NREPEATS=5000000;

#ifndef FACPARAM
#define FACPARAM 10
#endif

int main (int argc, char **argv) {
	int r = 0;

	if ( argc == 2 )
		NREPEATS = atoi ( argv[1] );

	Instruction* f = fac();

	int regs[NUM_REGS];
	regs[0] = FACPARAM;

	Timer cppTimer;
	for ( unsigned i = 0; i < NREPEATS; ++i ) {
		r = interpreter(f, 0, regs);
	}
	cppTimer.stop();

	std::cout << "For " << NREPEATS << " repeats\n";
	std::cout << "C++ Result: " << r << ", took " << cppTimer.getTime()
	          << " (" << (cppTimer.getTime()/NREPEATS) << " seconds per fac(" << FACPARAM << "), "
	          << (cppTimer.getTime()/(NREPEATS*(FACPARAM*4+3))) << " seconds per instruction)\n";

	Timer tgTimer;
	for ( unsigned i = 0; i < NREPEATS; ++i ) {
		r = interpreter_tg(f, 0, regs);
	}
	tgTimer.stop ( );

	std::cout << "TG Result: " << r << ", took " << tgTimer.getTime()
	          << " (" << (tgTimer.getTime()/NREPEATS) << " seconds per fac(" << FACPARAM << "), "
	          << (tgTimer.getTime()/(NREPEATS*(FACPARAM*4+3))) << " seconds per instruction)\n";
	theTimingInfo.print ( std::cout );
	return 0;
}

