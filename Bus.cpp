
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

	/*
	 * RAM address range. It covers 8kB of but there is only 2kB available
	 * so these 2kB are 'mirrored' through this address range. Using bitwise AND
	 * to mask the bottom 11Bits is the same as addr % 2048
	 */
	if(addr >= 0x000 && addr <=0x1FFF) {
		cpuRam[addr & 0x07FF] = data;
	}
}
uint8_t Bus::cpuRead(uint16_t addr, bool bReadOnly) {

	uint8_t data = 0x00; // temporary for now
	// check if address is in the correct range (cup addressable range)
	if(addr >= 0x0000 && addr <=0x1FFF) {
		// put the read data in the right variable and represent the mirroring every 2048 (0x07FF)
		data =  cpuRam[addr & 0x07FF];
	}
	return data;
}
