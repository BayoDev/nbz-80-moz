# <div align='center'> MO-Z replica</div>

# ⚠️ The original software is now recovered! This may still be helpful for use wihtout an 8251 ic installed ⚠️

<div align='center'> 

:warning: This is not fully working and the documentation is a work in progress :warning: </div>

This is a system monitor and debugger for the SGS ATES NBZ-80-S computer. The original monitor software (MO-Z) is not currently available so I decided to create a replica (freely) based on the original documentation.

The original MO-Z was intended for the CLZ-80 board as a replacement for the NC-Z.But this version (probably) won't run in its original configuration since it was developed on an NBZ-80-S without the 8251 IC.

1. [My configuration](#conf)
2. [Usage](#usage)
3. [Sources](#sources)

<a id='conf'></a>

# My Configuration

This software was originally developed on a machine with the following specs:

- 16k ram
- 8k rom partition (56~64k of the memory space, 2k eprom for each slot)
- RS232 transmission line (600 baud)

Check the original schematics for jumper configuration.

<a id='usage'></a>

# Usage

1. Connecting a terminal
2. Memory position & usage
3. Starting the monitor
4. Commands

## Connecting a terminal

:warning: Information may be inaccurate, check the manual for details :warning:

To use the MO-Z monitor you will need to hook up a serial terminal to the J5 connector. If all the jumpers are configured correctly the RS232 pins will be configured as follows

| PIN | USAGE |
|:---:|:-----:|
|  2  | TRANSMIT |
| 10  | GND |
| 18 | RECEIVE |

The BAUD rate can be changed as described in the manual and the configuration uses 1 start bit, 8 data bit and 2 stop bit.

## Memory position & usage

The software will take up 2kb of memory and it needs to be mapped starting from 0xF000. It also uses a section of RAM under address 0x0200 to store some variables, if some of these values are changed there will be problems with the monitor.

## Starting the monitor

_Currently_ the monitor must be used in pair with the NC-Z because you'll need it in order to launch the monitor.
Once the machine is powered on you need to load the value 0xF001 in the PC and press the GO key.

## Commands

For the commands refer back to https://archive.org/details/clz80.

### Implementation status

> :heavy_check_mark: = Fully implemented, :warning: = Partially implemented ,:x: = Not implemented

- (:heavy_check_mark:) "O" Open memory mode
- (:heavy_check_mark:) "R" Register mode    
- (:heavy_check_mark:) "P" Proceed
- (:heavy_check_mark:) "B" Breakpoint repeat
- (:warning:) "S" Single step
- (:warning:) "Tn" Trace steps
- (:warning:) "L" Serial load
- (:x:) "H" Parallel load
- (:x:) "C" Cassette load
- (:x:) "D" Dump
- (:x:) "A" Start assembler
- (:x:) "E" Start editor

## Notes on the "L" command

In my implementation of the "L" command the data is loaded starting at 0x0200 and the program will stop loading when it encounters three ";" symbols (0x3B 0x3B 0x3B)

<a id='sources'></a>

# Sources

1. https://archive.org/details/clz80
2. https://archive.org/details/sgs-ates-microcomputer-systems-clz-80/page/n7/mode/2up
