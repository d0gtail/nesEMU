################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../Bus.cpp \
../CPU6502.cpp \
../Cartridge.cpp \
../PPU2C02.cpp \
../main.cpp 

OBJS += \
./Bus.o \
./CPU6502.o \
./Cartridge.o \
./PPU2C02.o \
./main.o 

CPP_DEPS += \
./Bus.d \
./CPU6502.d \
./Cartridge.d \
./PPU2C02.d \
./main.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	g++ -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


