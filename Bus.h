
#ifndef BUS_H_
#define BUS_H_

#pragma once

#include <cstdint>
#include <array>
#include "CPU6502.h"
#include "PPU2C02.h"


class Bus {
public:
	Bus();
	virtual ~Bus();

public: // Devices on the BUS
	CPU6502 cpu; // the cpu
	PPU2C02 ppu; // the ppu

	// fake RAM to use temporarily
	std::array<uint8_t, 2048> cpuRam;

public: // BUS read / write
	/**
	 * no need to specify if we read or write to the BUS since it's implied by which of the methods one is calling
	 */
	void cpuWrite(uint16_t addr, uint8_t data); // writes DATA to ADDRESS
	uint8_t cpuRead(uint16_t addr, bool bReadOnly = false); // reads DATA from ADDRESS
};

#endif /* BUS_H_ */
