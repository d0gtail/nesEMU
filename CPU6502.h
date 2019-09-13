/*
 * CPU6502.h
 *
 *  Created on: 26.08.2019
 *      Author: holger
 */

#ifndef CPU6502_H_
#define CPU6502_H_

#pragma once

#include <cstdint>
#include <vector>
#include <string>

class Bus;

class CPU6502 {
public:
	CPU6502();
	virtual ~CPU6502();

	void ConnectBus(Bus *n ) { bus = n;}

	/*
	 * Status Register stores 8 Flags
	 */
	enum FLAGS6502 { // CPU Flags
		C = (1 << 0),	// Carry Bit
		Z = (1 << 1),	// Zero
		I = (1 << 2),	// Disable Interrupts
		D = (1 << 3), 	// Decimal Mode (unused in the NES)
		B = (1 << 4),	// Break
		U = (1 << 5),	// Unused
		V = (1 << 6),	// Overflow
		N = (1 << 7),	// Negative
	};

	/*
	 * CPU Core Registers exposed to public for easy access
	 */
	uint8_t status = 0x00; 	// Status Register
	uint8_t a = 0x00;		// Accumulator Register
	uint8_t x = 0x00;		// X Register
	uint8_t y = 0x00;		// Y Register
	uint8_t stkp = 0x00;	// Stack Pointer
	uint16_t pc = 0x0000;	// Program Counter

	/*
	 * external event methods
	 */
	void reset();	// Reset irq to bring the cpu into a known state
	void irq();		// Interrupt Request - executes an instruction at a specific location
	void nmi();		// Non-Maskable Interrupt Request - as IRQ but can't be disabled
	void clock();	// perform one clock cycle



private:
	Bus *bus = nullptr;

	void write(uint16_t addr, uint8_t data);
	uint8_t read(uint16_t addr);

	/*
	 * Base location of the STACK hard coded in the CPU = 0x0100
	 */
	uint16_t const STACKBASE = 0x0100;

	/*
	 * Assisting variables in the CPU
	 */
	uint8_t fetched 		= 0x00;		// Date which is fetched and used for the ALU
	uint16_t temp			= 0x0000;	// For convenience reasons
	uint16_t addr_abs		= 0x0000;	// Used memory addresses
	uint16_t addr_rel		= 0x00;		// Absolute address following a branch
	uint8_t	opcode			= 0x00;		// Instruction byte
	uint8_t	cycles			= 0;		// Counts the instructions remaining cycles
	uint32_t clock_count 	= 0;		// Global accumulation of clocks cycles

	/*
	 * If read location is provided immediately as part of the instruction
	 * the data must be fetched. This decision is dependent on the Address Mode
	 * in the instruction byte
	 */
	uint8_t fetch();

	/*
	 * Structure to save the opcode translation table. The 6502 CPU can have 256 instructions
	 * which are stored in a numerical order what makes them easy to look up.
	 */
	struct INSTRUCTION {
		std::string name; // Mnemonic: Text representation of the instruction (For disassembling)
		uint8_t (CPU6502::*operate )(void) = nullptr; // Function Pointer to the implementation of the opcode
		uint8_t (CPU6502::*addrmode)(void) = nullptr; // Function Pointer to the address mode
		uint8_t cycles = 0;	// Cycles which the CPU requires to perform the instruction
	};
	std::vector<INSTRUCTION> lookup;

private:
	/*
	 * Addressing Modes
	 * 6502 uses different addressing modes
	 * each opcode contains information about the addressing mode
	 * is needed to calculate the amount of bytes and cycles which are used by
	 * the opcodes
	 */
	uint8_t IMP();	uint8_t IMM();
	uint8_t ZP0();	uint8_t ZPX();
	uint8_t ZPY();	uint8_t REL();
	uint8_t ABS();	uint8_t ABX();
	uint8_t ABY();	uint8_t IND();
	uint8_t IZX();	uint8_t IZY();

	/*
	 * Opcodes
	 */
	uint8_t ADC(); uint8_t AND(); uint8_t ASL();
	uint8_t BCC(); uint8_t BCS(); uint8_t BEQ(); uint8_t BIT(); uint8_t BMI();
	uint8_t BNE(); uint8_t BPL(); uint8_t BRK(); uint8_t BVC(); uint8_t BVS();
	uint8_t CLC(); uint8_t CLD(); uint8_t CLI(); uint8_t CLV(); uint8_t CMP();
	uint8_t CPX(); uint8_t CPY();
	uint8_t DEC(); uint8_t DEX(); uint8_t DEY();
	uint8_t EOR();
	uint8_t INC(); uint8_t INX(); uint8_t INY();
	uint8_t JMP(); uint8_t JSR();
	uint8_t LDA(); uint8_t LDX(); uint8_t LDY(); uint8_t LSR();
	uint8_t NOP();
	uint8_t ORA();
	uint8_t PHA(); uint8_t PHP(); uint8_t PLA(); uint8_t PLP();
	uint8_t ROL(); uint8_t ROR(); uint8_t RTI(); uint8_t RTS();
	uint8_t SBC(); uint8_t SEC(); uint8_t SED(); uint8_t SEI(); uint8_t STA();
	uint8_t STX(); uint8_t STY();
	uint8_t TAX(); uint8_t TAY(); uint8_t TSX(); uint8_t TXA(); uint8_t TXS();
	uint8_t TYA();

	uint8_t XXX(); // illegal opcode

private:
	// convenience methods to access status register
	uint8_t GetFlag(FLAGS6502 f);
	void	SetFlag(FLAGS6502 f, bool v);
};

#endif /* CPU6502_H_ */
