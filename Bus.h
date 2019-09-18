
#ifndef BUS_H_
#define BUS_H_

#pragma once

#include <cstdint>
#include <array>
#include "CPU6502.h"


class Bus {
public:
	Bus();
	virtual ~Bus();

public: // Devices on the BUS
	CPU6502 cpu;

	// fake RAM to use temporarily
	// TODO: change with real RAM
	std::array<uint8_t, 64 * 1024> ram;

public: // BUS read / write
	/**
	 * no need to specify if we read or write to the BUS since it's implied by which of the methods one is calling
	 */
	void cpuWrite(uint16_t addr, uint8_t data); // writes DATA to ADDRESS
	uint8_t cpuRead(uint16_t addr, bool bReadOnly = false); // reads DATA from ADDRESS
};

#endif /* BUS_H_ */
