/*
 * CPU6502.cpp
 *
 *  Created on: 26.08.2019
 *      Author: holger
 */

#include "CPU6502.h"
#include "Bus.h"

CPU6502::CPU6502() {
	// TODO Auto-generated constructor stub

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
