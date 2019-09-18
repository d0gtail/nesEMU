
#include "Bus.h"

Bus::Bus() {

	// connect the cpu to the bus
	cpu.ConnectBus(this);

	// clear RAM
	for(auto &i : cpuRam) i = 0x00;

}

Bus::~Bus() {
	// TODO Auto-generated destructor stub
}

void Bus::cpuWrite(uint16_t addr, uint8_t data) {

	// check if address is in the correct range (the complete range for now)
	if(addr >= 0x000 && addr <=0xFFFF) {
		cpuRam[addr] = data;
	}
}
uint8_t Bus::cpuRead(uint16_t addr, bool bReadOnly) {

	// check if address is in the correct range (the complete range for now)
	if(addr >= 0x000 && addr <=0xFFFF) {
		return cpuRam[addr];
	}
	return 0x00;
}
