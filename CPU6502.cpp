/*
 * CPU6502.cpp
 *
 *  Created on: 26.08.2019
 *      Author: holger
 */

#include "CPU6502.h"
#include "Bus.h"

// Constructor
CPU6502::CPU6502() {
	/*
	 * Instruction translation table
	 * 16x16 entries -> 256 instructions where 56 of them are used as legal instructions
	 * Bottom 4 Bits -> column
	 * Top 4 Bits -> row
	 * initialiser list of initialiser lists
	 *
	 * MNEMONIC, Pointer to opcode, Pointer to Address Mode, Cycle count
	 * { "BRK",     &CPU6502::BRK,    &CPU6502::IMM,           7 }
	 */
	using a = CPU6502; // Alias to CPU6502 to make this list somewhat smaller
	lookup =
	{
			{ "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
			{ "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
			{ "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
			{ "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
			{ "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
			{ "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
			{ "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
			{ "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
			{ "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
			{ "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
			{ "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
			{ "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
			{ "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
			{ "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
			{ "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
			{ "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		};
}

CPU6502::~CPU6502() {
	// TODO Auto-generated destructor stub
}

void CPU6502::write(uint16_t addr, uint8_t data) {
	bus->write(addr, data);
}

uint8_t CPU6502::read(uint16_t addr) {
	return bus->read(addr, false);
}

void CPU6502::clock() {
	/*
	 * If the clock gets triggered we only going to perform any kind of operation
	 * when there are no cycles left from the last operation. Since we don't need
	 * to be that precise and modern computers are way faster than this old CPU we
	 * simply tick the leftover cycles down by doing nothing here.
	 */
	if(cycles == 0) {

		// read instruction byte at the program counter
		// and look it up in the translation table to see what the opcode has to do
		opcode = read(pc);
		// instruction has been read, incremet the program counter
		++pc;
		// get initial number of needed cycles
		cycles = lookup[opcode].cycles;
		// fetch data using the underlying addressing mode and store additional cycles
		uint8_t additionalCyleAddrMode = (this->*lookup[opcode].addrmode)();
		// perform the operation and again store more additional cycles
		uint8_t additionalCyclesOperation = (this->*lookup[opcode].operate)();
		// calculate overall cycles
		cycles += (additionalCyleAddrMode & additionalCyclesOperation);
	}
	// decrement the cycles since we performed the operation
	--cycles;
}

/*
 * Addressing Modes:
 *
 * The 6502 has an address range from 0x0000 - 0xFFFF. The high byte refers as "page" and
 * the low byte is the offset to that "page" which implies that there are 256 pages containing
 * 256 bytes each.
 *
 * Several Addressing Mode have the potential to need an additional clock cycle if they cross
 * "page" boundary. This combined with several instructions that enable this additional clock
 * cycle. Each address function returns a Flag indicating the potential to need an additional
 * cycle so does each instruction. If both of them return 1 as Flag an additional clock cycle
 * is required
 */

/* Addr. Mode: Implied
 * Doesn't need additional data. Does very simple tasks like setting an status bit. However,
 * we will use the accumulator for instructions like PHA.
 */
uint8_t CPU6502::IMP() {
	fetched = a; // storing the accumulator register in fetched
	return 0; // no additional cycle needed
}
/*
 * Addr. Mode: Immediate
 * The instruction expects the next byte as value so we set the read address to point to the
 * next byte
 */
uint8_t CPU6502::IMM(){
	addr_abs = pc++;
	return 0;
}
/*
 * Addr. Mode: Zero Page
 * To save time and bytes to use, zero page addressing absolute addresses the page zero.
 * This only requires one byte instead of two.
 */
uint8_t CPU6502::ZP0() {
	addr_abs = read(pc);
	++pc;
	addr_abs &= 0x00FF;
	return 0;
}
/*
 * Addr. Mode: Zero Page /w X offset
 * Like ZP0 but with the X Register added to the single byte address
 * Useful for iterating within the first page.
 */
uint8_t CPU6502::ZPX() {
	addr_abs = (read(pc) + x);
	++pc;
	addr_abs &= 0x00FF;
	return 0;
}
/*
 * Addr. Mode: Zero Page /w Y offset
 * Same as ZPX only with the Y Register used as offset
 */
uint8_t CPU6502::ZPY() {
	addr_abs = (read(pc) + y);
	++pc;
	addr_abs &= 0x00FF;
	return 0;
}
/*
 * Addr. Mode: Relative
 * Exclusive for branch instructions. The address must be within
 * -128 to +127 of the branching instruction. It cannot be branched
 * to any address in the addressable range directly.
 */
uint8_t CPU6502::REL() {
	addr_rel = read(pc);
	++pc;
	if(addr_rel & 0x80) { // check if address is signed
		addr_rel |= 0xFF00;
	}
	return 0;
}

/*
 * Addr. Mode: Absolute
 * Full 16-Bit address is used
 */
uint8_t CPU6502::ABS() {
	uint16_t loByte = read(pc);
	++pc;
	uint16_t hiByte = read(pc);
	++pc;
	addr_abs = ((hiByte << 8) | loByte);
	return 0;
}
/*
 * Addr. Mode: Absolute with X Offset
 * Same as Absolute address mode but with the content of the
 * X Register added. If the resulting address changes the page,
 * an additional clock cycle is required
 */
uint8_t CPU6502::ABX() {
	uint16_t loByte = read(pc);
	++pc;
	uint16_t hiByte = read(pc);
	++pc;
	addr_abs = ((hiByte << 8) | loByte);
	addr_abs += this->x;

	// hiByte shifted by 8 to the left represents the page index
	// if the resulting high part of addr_abs isn't the same as the high byte
	// the page index has changed so we have throw an extra clock cycle in the equation
	if((addr_abs & 0xFF00) != (hiByte << 8)) {
		return 1;
	}else{
		return 0;
	}
}
/*
 * Addr. Mode: Absolute with Y Offset
 * Same as ABX but with the Y Register
 */
uint8_t CPU6502::ABY() {
	uint16_t loByte = read(pc);
	++pc;
	uint16_t hiByte = read(pc);
	++pc;
	addr_abs = ((hiByte << 8) | loByte);
	addr_abs += this->y;

	// hiByte shifted by 8 to the left represents the page index
	// if the resulting high part of addr_abs isn't the same as the high byte
	// the page index has changed so we have throw an extra clock cycle in the equation
	if((addr_abs & 0xFF00) != (hiByte << 8)) {
		return 1;
	}else{
		return 0;
	}
}
// Next 3 address modes use indirection (aka Pointers)
/*
 * Addr. Mode: Indirect Addressing
 * 16-Bit Address supplied which is a pointer which has to be dereferenced.
 * In the hardware this instruction has a bug and to emulate this function correct we
 * need to emulate this bug.
 * If the low byte of the supplied address is 0xFF, then to read the high byte of the
 * actual address we need to cross a page boundary. This doesn't actually happen on the
 * chip as designed, instead it wraps back around in the same page, yielding an invalid
 * actual address
 */
uint8_t CPU6502::IND() {
	uint16_t ptrLo = read(pc);
	++pc;
	uint16_t ptrHi = read(pc);
	++pc;

	uint16_t ptr = ((ptrHi << 8) | ptrLo);

	if(ptrLo == 0x00FF) {// Simulate page boundary hardware bug
		addr_abs = (read(ptr & 0xFF00) << 8 | read(ptr + 0));
	}else{ // Behave normally
		addr_abs = (read(ptr + 1) << 8 | read(ptr + 0));
	}
	return 0;
}
/*
 * Addr. Mode: Indirect X
 * Supplied 8-Bit Address is offset by X Register to index a location
 * in page 0x00. The actual 16-Bit Address is read from this location
 */
uint8_t CPU6502::IZX() {
	uint16_t temp = read(pc);
	++pc;

	uint16_t ptrHi = read((uint16_t)(temp + (uint16_t)this->x) & 0x00FF);
	uint16_t ptrLo = read((uint16_t)(temp + (uint16_t)this->x + 1) & 0x00FF);

	addr_abs = ((ptrHi << 8) | ptrLo);

	return 0;
}
/*
 * Addr. Mode: Indirect Y
 * The supplied 8-Bit address indexes a location in page 0x00.
 * From there the actual 16-Bit address is read, and the contents
 * of Y Register is added to it as offset. If the offset causes a change
 * in page then an additional clock cycle is needed.
 */
uint8_t CPU6502::IZY() {
	uint16_t temp = read(pc);
	++pc;

	uint16_t ptrLo = read(temp & 0x00FF);
	uint16_t ptrHi = read((temp + 1) & 0x00FF);

	addr_abs = ((ptrHi << 8) | ptrLo);
	addr_abs += this->y;

	if((addr_abs & 0xFF00) != (ptrHi << 8)) {
		return 1;
	}else{
		return 0;
	}
}

/*
 * Instructions: fetched
 *
 * This function grabs data used by the instruction into a
 * convenient variable. Some instructions don't have to fetch
 * data as it's implied by the instruction. E.g. "INX"
 * increments the X Register, where no additional data is
 * required. For all other addressing modes the data is located
 * where addr_abs points to so it's read from there. Immidiate
 * address mode exploits this slightly, as that has the data at pc + 1
 * so it fetches the data from the next byte.
 *
 */
uint8_t CPU6502::fetch() {
	if(!(lookup[opcode].addrmode == &CPU6502::IMP())) {
		this->fetched = read(addr_abs);
	}
	return this->fetched;
}
/*
 * Instruction: AND
 * Bitwise logic and
 * Function:	A = A & M
 * Flags Out:	N, Z
 */
uint8_t CPU6502::AND() {
	fetch(); // first fetch the data and store it in fetched
	this->a = this->a & this->fetched; // and the data with the accumulator and store it in the accu.
	SetFlag(Z, this->a == 0x00);
	SetFlag(N, a & 0x80);
	return 1; // potential candidate to add additional clock cycle
}
