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
	 * Initializer list of initializer lists
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
/*
 * External Inputs
 *
 * Reset sets the CPU in a known state
 *
 * Status register is cleared, except of the unused Bit which stays 1
 * An absolute address is read from 0xFFFC which contains an further address
 * which is used to set the Program Counter to. The value at 0xFFFC may be set
 * at compile time to have an known starting point.
 */
void CPU6502::reset() {
	// Get the address to set the PC to
	uint16_t rstLo = read(this->RESET_PC + 0);
	uint16_t rstHi = read(this->RESET_PC + 1);
	// Set the PC
	this->pc = (rstHi << 8 | rstLo);

	// Reset internal registers
	this->a = 0x00;
	this->x = 0x00;
	this->y = 0x00;
	this->stkp = this->RESET_STKP;
	this->status = 0x00 | this->U;

	// Reset internal helpers
	this->addr_abs = 0x0000;
	this->addr_rel = 0x0000;
	this->fetched = 0x00;

	// Reset needs 8 cycles
	this->cycles = 8;
}
/*
 * Interrupt request immediate attention from the CPU but only when the
 * disable interrupt flag is 0
 */
void CPU6502::irq() {
	if(GetFlag(this->I) == 0) {
		// Push the PC to the stack
		write(this->STACKBASE + this->stkp, (pc >> 8) & 0x00FF);
		--this->stkp;
		write(this->STACKBASE + this->stkp, pc & 0x00FF);
		--this->stkp;

		// Push status register to the stack
		SetFlag(this->B, 0);
		SetFlag(this->U, 1);
		SetFlag(this->I, 1);
		write(this->STACKBASE + this->stkp, this->status);
		--this->stkp;

		// Read new PC location from fixed address
		uint16_t irqLo = read(this->IRQ_PC + 0);
		uint16_t irqHi = read(this->IRQ_PC + 1);

		this->pc = (irqHi << 8) | irqLo;

		// IRQ needs 7 cycles
		this->cycles = 7;
	}
}
void CPU6502::clock() {
	/*
	 * If the clock gets triggered we only going to perform any kind of operation
	 * when there are no cycles left from the last operation. Since we don't need
	 * to be that precise and modern computers are way faster than this old CPU we
	 * simply tick the leftover cycles down by doing nothing here.
	 */
	if(this->cycles == 0) {

		// read instruction byte at the program counter
		// and look it up in the translation table to see what the opcode has to do
		this->opcode = read(this->pc);
		// instruction has been read, increment the program counter
		++this->pc;
		// get initial number of needed cycles
		this->cycles = lookup[this->opcode].cycles;
		// fetch data using the underlying addressing mode and store additional cycles
		uint8_t additionalCyleAddrMode = (this->*lookup[this->opcode].addrmode)();
		// perform the operation and again store more additional cycles
		uint8_t additionalCyclesOperation = (this->*lookup[this->opcode].operate)();
		// calculate overall cycles
		this->cycles += (additionalCyleAddrMode & additionalCyclesOperation);
	}
	// decrement the cycles since we performed the operation
	--this->cycles;
}
/*
 * FLAG FUNCTIONS:
 *
 * GetFlag: Returns the value of a specific Bit of the status register
 */
uint8_t CPU6502::GetFlag(FLAGS6502 f) {
	return ((this->status & f) > 0) ? 1 : 0;
}
/*
 * SetFlag: Sets of clears a specific Bit of the status register
 */
void CPU6502::SetFlag(FLAGS6502 f, bool v) {
	if(v) {
		this->status |= f;
	}else{
		this->status &= ~f;
	}
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
	this->fetched = a; // storing the accumulator register in this->fetched
	return 0; // no additional cycle needed
}
/*
 * Addr. Mode: Immediate
 * The instruction expects the next byte as value so we set the read address to point to the
 * next byte
 */
uint8_t CPU6502::IMM(){
	this->addr_abs = ++this->pc;
	return 0;
}
/*
 * Addr. Mode: Zero Page
 * To save time and bytes to use, zero page addressing absolute addresses the page zero.
 * This only requires one byte instead of two.
 */
uint8_t CPU6502::ZP0() {
	this->addr_abs = read(this->pc);
	++this->pc;
	this->addr_abs &= 0x00FF;
	return 0;
}
/*
 * Addr. Mode: Zero Page /w X offset
 * Like ZP0 but with the X Register added to the single byte address
 * Useful for iterating within the first page.
 */
uint8_t CPU6502::ZPX() {
	this->addr_abs = (read(this->pc) + x);
	++this->pc;
	this->addr_abs &= 0x00FF;
	return 0;
}
/*
 * Addr. Mode: Zero Page /w Y offset
 * Same as ZPX only with the Y Register used as offset
 */
uint8_t CPU6502::ZPY() {
	this->addr_abs = (read(this->pc) + y);
	++this->pc;
	this->addr_abs &= 0x00FF;
	return 0;
}
/*
 * Addr. Mode: Relative
 * Exclusive for branch instructions. The address must be within
 * -128 to +127 of the branching instruction. It cannot be branched
 * to any address in the addressable range directly.
 */
uint8_t CPU6502::REL() {
	this->addr_rel = read(this->pc);
	++this->pc;
	if(this->addr_rel & 0x80) { // check if address is signed
		this->addr_rel |= 0xFF00;
	}
	return 0;
}

/*
 * Addr. Mode: Absolute
 * Full 16-Bit address is used
 */
uint8_t CPU6502::ABS() {
	uint16_t loByte = read(this->pc);
	++this->pc;
	uint16_t hiByte = read(this->pc);
	++this->pc;
	this->addr_abs = ((hiByte << 8) | loByte);
	return 0;
}
/*
 * Addr. Mode: Absolute with X Offset
 * Same as Absolute address mode but with the content of the
 * X Register added. If the resulting address changes the page,
 * an additional clock cycle is required
 */
uint8_t CPU6502::ABX() {
	uint16_t loByte = read(this->pc);
	++this->pc;
	uint16_t hiByte = read(this->pc);
	++this->pc;
	this->addr_abs = ((hiByte << 8) | loByte);
	this->addr_abs += this->x;

	// hiByte shifted by 8 to the left represents the page index
	// if the resulting high part of addr_abs isn't the same as the high byte
	// the page index has changed so we have throw an extra clock cycle in the equation
	if((this->addr_abs & 0xFF00) != (hiByte << 8)) {
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
	uint16_t loByte = read(this->pc);
	++this->pc;
	uint16_t hiByte = read(this->pc);
	++this->pc;
	this->addr_abs = ((hiByte << 8) | loByte);
	this->addr_abs += this->y;

	// hiByte shifted by 8 to the left represents the page index
	// if the resulting high part of addr_abs isn't the same as the high byte
	// the page index has changed so we have throw an extra clock cycle in the equation
	if((this->addr_abs & 0xFF00) != (hiByte << 8)) {
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
	uint16_t ptrLo = read(this->pc);
	++this->pc;
	uint16_t ptrHi = read(this->pc);
	++this->pc;

	uint16_t ptr = ((ptrHi << 8) | ptrLo);

	if(ptrLo == 0x00FF) {// Simulate page boundary hardware bug
		this->addr_abs = (read(ptr & 0xFF00) << 8 | read(ptr + 0));
	}else{ // Behave normally
		this->addr_abs = (read(ptr + 1) << 8 | read(ptr + 0));
	}
	return 0;
}
/*
 * Addr. Mode: Indirect X
 * Supplied 8-Bit Address is offset by X Register to index a location
 * in page 0x00. The actual 16-Bit Address is read from this location
 */
uint8_t CPU6502::IZX() {
	uint16_t temp = read(this->pc);
	++this->pc;

	uint16_t ptrHi = read((uint16_t)(temp + (uint16_t)this->x) & 0x00FF);
	uint16_t ptrLo = read((uint16_t)(temp + (uint16_t)this->x + 1) & 0x00FF);

	this->addr_abs = ((ptrHi << 8) | ptrLo);

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
	uint16_t temp = read(this->pc);
	++this->pc;

	uint16_t ptrLo = read(temp & 0x00FF);
	uint16_t ptrHi = read((temp + 1) & 0x00FF);

	this->addr_abs = ((ptrHi << 8) | ptrLo);
	this->addr_abs += this->y;

	if((this->addr_abs & 0xFF00) != (ptrHi << 8)) {
		return 1;
	}else{
		return 0;
	}
}

/*
 * Instructions: this->fetched
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
	if(!(lookup[this->opcode].addrmode == &CPU6502::IMP)) {
		this->fetched = read(this->addr_abs);
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
	fetch(); // first fetch the data and store it in this->fetched
	this->a = this->a & this->fetched; // and the data with the accumulator and store it in the accu.
	SetFlag(Z, this->a == 0x00);
	SetFlag(N, a & 0x80);
	return 1; // potential candidate to add additional clock cycle
}
/*
 * Instruction: BCC
 * Branch when Carry is Clear
 * Function:	if(c==0) pc = address
 */
uint8_t CPU6502::BCC() {
	if(this->GetFlag(C) == 0) {
		++this->cycles; // add one additional cycle if branch to the same page (so at least one has to be added)
		this->addr_abs = this->pc + this->addr_rel;

		if((this->addr_abs & 0xFF00) != (this->pc & 0xFF00)) {
			++this->cycles; // add another clock cycle if page boundarys are crossed
		}
		this->pc = this->addr_abs;
	}
	return 0;
}
/*
 * Instruction: BCS
 * Branch when Carry is Set
 * Function:	if(c==1) pc = address
 */
uint8_t CPU6502::BCS() {
	if(this->GetFlag(C) == 1) {
		++this->cycles;
		this->addr_abs = this->pc + this->addr_rel;

		if((this->addr_abs & 0xFF00) != (this->pc & 0xFF00)) {
			++this->cycles;
		}
		this->pc = this->addr_abs;
	}
	return 0;
}
/*
 * Instruction: BNE
 * Branch not Equal
 * Function:	if(Z == 0) pc = address
 */
uint8_t CPU6502::BNE() {
	if(this->GetFlag(Z) == 0) {
		++this->cycles;
		this->addr_abs = this->pc + this->addr_rel;

		if((this->addr_abs & 0xFF00) != (this->pc & 0xFF00)) {
			++this->cycles;
		}
		this->pc = this->addr_abs;
	}
	return 0;
}
/*
 * Instruction: BEQ
 * Branch if Equal
 * Function:	if(Z == 1) pc = address
 */
uint8_t CPU6502::BEQ() {
	if(this->GetFlag(Z) == 1) {
		++this->cycles;
		this->addr_abs = this->pc + this->addr_rel;

		if((this->addr_abs & 0xFF00) != (this->pc & 0xFF00)) {
			++this->cycles;
		}
		this->pc = this->addr_abs;
	}
	return 0;
}
/*
 * Instruction: BMI
 * Branch if Negative
 * Function:	if(N == 1) pc = address
 */
uint8_t CPU6502::BMI() {
	if(this->GetFlag(N) == 1) {
		++this->cycles;
		this->addr_abs = this->pc + this->addr_rel;

		if((this->addr_abs & 0xFF00) != (this->pc & 0xFF00)) {
			++this->cycles;
		}
		this->pc = this->addr_abs;
	}
	return 0;
}
/*
 * Instruction: BPL
 * Branch if Positive
 * Function:	if(N == 0) pc = address
 */
uint8_t CPU6502::BPL() {
	if(GetFlag(N) == 0) {
		++this->cycles;
		this->addr_abs = this->pc + this->addr_rel;

		if((this->addr_abs & 0xFF00) != (this->pc & 0xFF00)) {
			++this->cycles;
		}
		this->pc = this->addr_abs;
	}
	return 0;
}
/*
 * Instruction: BVC
 * Branch if Overflow is Clear
 * Function:	if(V == 0) pc = address
 */
uint8_t CPU6502::BVC() {
	if(GetFlag(V) == 0) {
		++this->cycles;
		this->addr_abs = this->pc + this->addr_rel;

		if((this->addr_abs & 0xFF00) != (this->pc & 0xFF00)) {
			++this->cycles;
		}
		this->pc = this->addr_abs;
	}
	return 0;
}

/*
 * Instruction: CLC
 * Clear carry Flag
 * Function:	C = 0;
 */
uint8_t CPU6502::CLC() {
	SetFlag(C, false);
	return 0;
}
/*
 * Instruction: CLD
 * Clear Decimal Flag
 * Function:	D = 0;
 */
uint8_t CPU6502::CLD() {
	SetFlag(D, false);
	return 0;
}
/*
 * Instruction: CLI
 * Disable Interrupts / Clear Interrupt Flag
 * Function:	I = 0;
 */
uint8_t CPU6502::CLI() {
	SetFlag(I, false);
	return 0;
}
/* Instruction: CLV
 * Clear Overflow Flag
 * Function:	V = 0;
 */
uint8_t CPU6502::CLV() {
	SetFlag(V, false);
	return 0;
}
/* Instruction: ADC
 * Addition with special attention to the C, Z, V, N Flags
 * Function:	A += M + C
 * Flags:		C if result is > 255
 * 				Z if result is 0
 * 				V if ~(A^M) & (A^R) // R -> Result
 * Flags base on the following hypothesis:
 * Pos + Pos = Neg -> Overflow
 * Neg + Neg = Pos -> Overflow
 * Pos + Neg = All -> cannot overflow
 * Pos + Pos = Pos -> No Overflow
 * Neg + Neg = Neg -> No Overflow
 */
uint8_t CPU6502::ADC() {

	this->fetch(); // grab the data we want to add

	/*
	 * The actual addition is performed in 16-Bit Words to capture
	 * any set Carry Bit, which will be located in Bit 8 of the word.
	 */
	this->temp = (uint16_t)this->a + (uint16_t)this->fetched + (uint16_t)GetFlag(C);

	SetFlag(C, this->temp > 255); // Set Carry Flag if high byte bit is 0

	SetFlag(Z, (this->temp & 0x00FF) == 0); // Zero Flag if result is zero

	/*
	 * Overflow Flag is set accordingly to the methods description
	 */
	SetFlag(V, (~((uint16_t)this->a ^ (uint16_t)this->fetched) & ((uint16_t)this->a ^ (uint16_t)this->temp)) & 0x0080);

	SetFlag(N, (this->temp & 0x0080)); // Negative Flag is set to the MSB of the result

	this->a = this->temp & 0x00FF; // Put the result in the accumulator register

	return 1; // This instruction has the potential to add an clock cycle
}
/*
 * Instruction SBC
 * Subtraction with special attention to the C, Z, V, N Flags
 * Function:		A -= M-(1 - C), A += -1(M-(1-C)), A += -M + 1 + C
 * Flags:			C, V, N, Z
 *
 * After reordering the mathematical function we end up with an addition with the data negated
 */
uint8_t CPU6502::SBC() {
	this->fetch();

	uint16_t val = ((uint16_t)this->fetched) ^ 0x00FF; // Invert the bottom 8-Bits with xor

	/*
	 * From here on we do the same thing as in ADC
	 */
	this->temp = (uint16_t)this->a + val + (uint16_t)GetFlag(C);

	SetFlag(C, this->temp > 255); // Set Carry Flag if high byte bit is 0

	SetFlag(Z, (this->temp & 0x00FF) == 0); // Zero Flag if result is zero

	SetFlag(V, (this->temp ^ (uint16_t)this->a) & (this->temp ^ val) & 0x0080);

	SetFlag(N, this->temp & 0x0080);

	this->a = this->temp & 0x00FF;

	return 1;
}
/*
 * Instruction PHA
 * Push Accumulator to Stack
 * Function:	A -> Stack
 */
uint8_t CPU6502::PHA() {
	write(this->STACKBASE + this->stkp, this->a);
	--this->stkp;
	return 0;
}
/*
 * Instruction PHP
 * Push status Register to Stack
 * Function:	status -> Stack
 * Note:		Break Flag is set to 1 before push
 */
uint8_t CPU6502::PHP() {
	write(this->STACKBASE + this->stkp, this->status | B | U); // Set Break to 1 before push
	SetFlag(B, 0);
	SetFlag(U, 0);
	--this->stkp;
	return 0;
}
/*
 * Instruction PLA
 * Pop Accumulator from the Stack
 * Function:	A <- Stack
 */
uint8_t CPU6502::PLA() {
	++this->stkp;
	this->a = read(this->STACKBASE + this->stkp);
	SetFlag(Z, this->a == 0x00);
	SetFlag(N, this->a & 0x80);
	return 0;
}
/*
 * Instruciton RTI
 * Return from Interrupt
 * Reverse the Status of the CPU to what it was before the IRQ
 */
uint8_t CPU6502::RTI() {
	++this->stkp;
	this->status = read(this->STACKBASE + this->stkp);
	this->status &= ~this->B;
	this->status &= ~this->U;

	++this->stkp;
	this->pc = (uint16_t)read(this->STACKBASE + this->stkp);
	++this->stkp;
	this->pc |= (uint16_t)read((this->STACKBASE + this->stkp) << 8);
	return 0;
}
