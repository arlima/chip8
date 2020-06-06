package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"time"

	"github.com/veandco/go-sdl2/sdl"
)

const (
	windowTitle  = "CHIP-8 Emulator"
	windowWidth  = 64 * 10
	windowHeight = 32 * 10
)

type chip8 struct {
	memory     [4096]uint8
	v          [16]uint8
	vi         uint16
	pc         uint16
	screen     [64][32]uint8
	delayTimer uint8
	soundTimer uint8
	stack      [16]uint16
	sp         uint16
	drawFlag   uint16
	key        [16]uint8
	opcodes    map[uint16]opcodeHandler
	keycodes   map[sdl.Keycode]int
}

var chip8Fontset = [80]uint8{
	0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
	0x20, 0x60, 0x20, 0x20, 0x70, // 1
	0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
	0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
	0x90, 0x90, 0xF0, 0x10, 0x10, // 4
	0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
	0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
	0xF0, 0x10, 0x20, 0x40, 0x40, // 7
	0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
	0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
	0xF0, 0x90, 0xF0, 0x90, 0x90, // A
	0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
	0xF0, 0x80, 0x80, 0x80, 0xF0, // C
	0xE0, 0x90, 0x90, 0x90, 0xE0, // D
	0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
	0xF0, 0x80, 0xF0, 0x80, 0x80} // F

// opcodeHandler processes an opcode and returns an error if the opcode could not be handled.
type opcodeHandler func(opcode uint16) error

// registerOpcodeHandlers maps opcode handler functions to the 0xF000 mask
// of the opcodes that they handle
func (c *chip8) registerOpcodeHandlers() {
	c.opcodes = map[uint16]opcodeHandler{
		0x0000: c.opcode0x0000,
		0x1000: c.opcode0x1000,
		0x2000: c.opcode0x2000,
		0x3000: c.opcode0x3000,
		0x4000: c.opcode0x4000,
		0x5000: c.opcode0x5000,
		0x6000: c.opcode0x6000,
		0x7000: c.opcode0x7000,
		0x8000: c.opcode0x8000,
		0x9000: c.opcode0x9000,
		0xA000: c.opcode0xA000,
		0xB000: c.opcode0xB000,
		0xC000: c.opcode0xC000,
		0xD000: c.opcode0xD000,
		0xE000: c.opcode0xE000,
		0xF000: c.opcode0xF000,
	}
}

func (c *chip8) registerKeyCodes() {
	c.keycodes = map[sdl.Keycode]int{
		sdl.K_1: 0x1,
		sdl.K_2: 0x2,
		sdl.K_3: 0x3,
		sdl.K_4: 0xC,
		sdl.K_q: 0x4,
		sdl.K_w: 0x5,
		sdl.K_e: 0x6,
		sdl.K_r: 0xD,
		sdl.K_a: 0x7,
		sdl.K_s: 0x8,
		sdl.K_d: 0x9,
		sdl.K_f: 0xE,
		sdl.K_z: 0xA,
		sdl.K_x: 0x0,
		sdl.K_c: 0xB,
		sdl.K_v: 0xF,
	}
}

func (c *chip8) loadGame(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	stats, statsErr := file.Stat()
	if statsErr != nil {
		return statsErr
	}

	var size int64 = stats.Size()

	if size > (4096 - 0x200) {
		return fmt.Errorf("the ROM file is too big")
	}

	bytes := make([]uint8, size)

	bufr := bufio.NewReader(file)
	_, err = bufr.Read(bytes)

	for m := int64(0); m < size; m++ {
		c.memory[0x200+m] = bytes[m]
	}

	return nil
}

func (c *chip8) initialize() {
	c.pc = 0x200
	c.vi = 0
	c.sp = 0
	for m := 0; m < 80; m++ {
		c.memory[m] = chip8Fontset[m]
	}

	for k := 0; k < 16; k++ {
		c.key[k] = 0
	}

	c.registerOpcodeHandlers()
	c.registerKeyCodes()
}

func (c *chip8) printMemory() {
	for m := 0; m < 4096; m++ {
		fmt.Printf("%X ", c.memory[m])
	}
}

// opcode0x0000 handles opcodes for clearing the display and returning
// from the stack
func (c *chip8) opcode0x0000(opcode uint16) error {

	switch opcode & 0x0FFF {
	case 0x0E0: // disp_clear(); Clears the screen.
		for gx := 0; gx < 64; gx++ {
			for gy := 0; gy < 32; gy++ {
				c.screen[gx][gy] = 0
			}
		}
		c.pc += 2
		c.drawFlag = 1
	case 0x0EE: // return; Returns from a subroutine.
		c.sp--
		c.pc = c.stack[c.sp]
	default:
		fmt.Printf("Unknown opcode: 0x%X\n", opcode)
	}

	return nil
}

// goto NNN; Jumps to address NNN.
func (c *chip8) opcode0x1000(opcode uint16) error {
	c.pc = opcode & 0x0FFF
	return nil
}

// *(0xNNN)(); Calls subroutine at NNN.
func (c *chip8) opcode0x2000(opcode uint16) error {
	c.stack[c.sp] = c.pc + 2
	c.sp++
	c.pc = opcode & 0x0FFF
	return nil
}

// if(Vx==NN); Skips the next instruction if VX equals NN. (Usually the next instruction is a jump to skip a code block)
func (c *chip8) opcode0x3000(opcode uint16) error {
	if c.v[(opcode&0x0F00)>>8] == uint8((opcode & 0x00FF)) {
		c.pc += 4
	} else {
		c.pc += 2
	}
	return nil
}

// if(Vx!=NN); Skips the next instruction if VX doesn't equal NN. (Usually the next instruction is a jump to skip a code block)
func (c *chip8) opcode0x4000(opcode uint16) error {
	if c.v[(opcode&0x0F00)>>8] != uint8((opcode & 0x00FF)) {
		c.pc += 4
	} else {
		c.pc += 2
	}
	return nil
}

// if(Vx==Vy); Skips the next instruction if VX equals VY. (Usually the next instruction is a jump to skip a code block)
func (c *chip8) opcode0x5000(opcode uint16) error {
	if c.v[(opcode&0x0F00)>>8] == c.v[(opcode&0x00F0)>>4] {
		c.pc += 4
	} else {
		c.pc += 2
	}
	return nil
}

// Vx = NN; Sets VX to NN.
func (c *chip8) opcode0x6000(opcode uint16) error {
	c.v[(opcode&0x0F00)>>8] = uint8((opcode & 0x00FF))
	c.pc += 2
	return nil
}

// Vx += NN; Adds NN to VX. (Carry flag is not changed)
func (c *chip8) opcode0x7000(opcode uint16) error {
	c.v[(opcode&0x0F00)>>8] += uint8((opcode & 0x00FF))
	c.pc += 2
	return nil
}

func (c *chip8) opcode0x8000(opcode uint16) error {
	x := (opcode & 0x0F00) >> 8
	y := (opcode & 0x00F0) >> 4
	n := (opcode & 0x000F)

	switch n {
	case 0: // Vx=Vy; Sets VX to the value of VY.
		c.v[x] = c.v[y]
	case 1: // Vx=Vx|Vy; Sets VX to VX or VY. (Bitwise OR operation)
		c.v[x] = c.v[x] | c.v[y]
	case 2: // Vx=Vx&Vy; Sets VX to VX and VY. (Bitwise AND operation)
		c.v[x] = c.v[x] & c.v[y]
	case 3: // Vx=Vx^Vy; Sets VX to VX xor VY.
		c.v[x] = c.v[x] ^ c.v[y]
	case 4: // Vx += Vy; Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
		if c.v[y] > 0xFF-c.v[x] {
			c.v[0xF] = 1
		} else {
			c.v[0xF] = 0
		}
		c.v[x] += c.v[y]
	case 5: // Vx -= Vy; VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
		if c.v[y] > c.v[x] {
			c.v[0xF] = 0
		} else {
			c.v[0xF] = 1
		}
		c.v[x] -= c.v[y]
	case 6: // Vx>>=1; Stores the least significant bit of VX in VF and then shifts VX to the right by 1.
		c.v[0xF] = uint8(c.v[x] & 0x1)
		c.v[x] = c.v[x] >> 1
	case 7: // Vx=Vy-Vx; Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
		if c.v[x] > c.v[y] {
			c.v[0xF] = 0
		} else {
			c.v[0xF] = 1
		}
		c.v[x] = c.v[y] - c.v[x]
	case 0xE: // Vx<<=1; Stores the most significant bit of VX in VF and then shifts VX to the left by 1.
		c.v[0xF] = uint8(c.v[x]) >> 7
		c.v[x] = c.v[x] << 1
	default:
		fmt.Printf("Unknown opcode: 0x%X\n", opcode)
	}
	c.pc += 2
	return nil
}

// if(Vx!=Vy); Skips the next instruction if VX doesn't equal VY. (Usually the next instruction is a jump to skip a code block)
func (c *chip8) opcode0x9000(opcode uint16) error {
	if c.v[(opcode&0x0F00)>>8] != c.v[(opcode&0x00F0)>>4] {
		c.pc += 4
	} else {
		c.pc += 2
	}
	return nil
}

// I = NNN; Sets I to the address NNN.
func (c *chip8) opcode0xA000(opcode uint16) error {
	c.vi = (opcode & 0x0FFF)
	c.pc += 2
	return nil
}

// PC=V0+NNN; Jumps to the address NNN plus V0.
func (c *chip8) opcode0xB000(opcode uint16) error {
	c.pc = uint16(c.v[0]) + (opcode & 0x0FFF)
	return nil
}

// Vx=rand()&NN; Sets VX to the result of a bitwise and operation on a random number (Typically: 0 to 255) and NN.
func (c *chip8) opcode0xC000(opcode uint16) error {
	c.v[(opcode&0x0F00)>>8] = uint8(rand.Intn(256)) & uint8((opcode & 0x00FF))
	c.pc += 2
	return nil
}

// draw(Vx,Vy,N); Draws a sprite at coordinate (VX, VY) that has a width of 8 pixels and a height of N pixels. Each
// row of 8 pixels is read as bit-coded starting from memory location I; I value doesn’t change after
// the execution of this instruction. As described above, VF is set to 1 if any screen pixels are flipped
// from set to unset when the sprite is drawn, and to 0 if that doesn’t happen
func (c *chip8) opcode0xD000(opcode uint16) error {
	var px, py, vx, vy uint16
	var pixel uint8
	x := (opcode & 0x0F00) >> 8
	y := (opcode & 0x00F0) >> 4
	n := (opcode & 0x000F)
	vx = uint16(c.v[x])
	vy = uint16(c.v[y])
	c.v[0xF] = 0
	for py = 0; py < n; py++ {
		pixel = c.memory[c.vi+py]
		for px = 0; px < 8; px++ {
			if vx+px > 63 || vy+py > 31 {
				continue
			}
			if (pixel & (0x80 >> px)) != 0 {
				if c.screen[vx+px][vy+py] == 1 {
					c.v[0xF] = 1
				}
				c.screen[vx+px][vy+py] ^= 1
			}
		}
	}
	c.pc += 2
	c.drawFlag = 1
	return nil
}

func (c *chip8) opcode0xE000(opcode uint16) error {
	switch opcode & 0x00FF {
	case 0x9E: // if(key()==Vx); Skips the next instruction if the key stored in VX is pressed. (Usually the next instruction is a jump to skip a code block)
		if c.key[c.v[(opcode&0x0F00)>>8]] == 1 {
			c.pc += 4
		} else {
			c.pc += 2
		}
	case 0xA1: // if(key()!=Vx); Skips the next instruction if the key stored in VX isn't pressed. (Usually the next instruction is a jump to skip a code block)
		if c.key[c.v[(opcode&0x0F00)>>8]] == 0 {
			c.pc += 4
		} else {
			c.pc += 2
		}
	default:
		fmt.Printf("Unknown opcode: 0x%X\n", opcode)
	}
	return nil
}

func (c *chip8) opcode0xF000(opcode uint16) error {
	x := (opcode & 0x0F00) >> 8
	nn := (opcode & 0x00FF)

	switch nn {
	case 0x07: // Vx = get_delay(); Sets VX to the value of the delay timer.
		c.v[x] = c.delayTimer
	case 0x0A: // Vx = get_key(); A key press is awaited, and then stored in VX. (Blocking Operation. All instruction halted until next key event)
		pressed := false
		for i := 0; i < len(c.key); i++ {
			if c.key[i] != 0 {
				c.v[x] = uint8(i)
				pressed = true
			}
		}
		if !pressed {
			return nil
		}
	case 0x15: // delay_timer(Vx); Sets the delay timer to VX.
		c.delayTimer = c.v[x]
	case 0x18: // sound_timer(Vx); Sets the sound timer to VX.
		c.soundTimer = c.v[x]
	case 0x1E: // I += Vx; Adds VX to I. VF is set to 1 when there is a range overflow (I+VX>0xFFF), and to 0 when there isn't.
		if c.vi > 0xFFF-uint16(c.v[x]) {
			c.v[0xF] = 1
		} else {
			c.v[0xF] = 0
		}
		c.vi += uint16(c.v[x])
	case 0x29: // I=sprite_addr[Vx]; Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
		c.vi = uint16(c.v[x]) * 5

	// Stores the binary-coded decimal representation of VX, with the most significant of three digits
	// at the address in I, the middle digit at I plus 1, and the least significant digit at I plus 2.
	// (In other words, take the decimal representation of VX, place the hundreds digit in memory at location in I,
	// the tens digit at location I+1, and the ones digit at location I+2.)
	case 0x33:
		c.memory[c.vi] = c.v[x] / 100
		c.memory[c.vi+1] = (c.v[x] / 10) % 10
		c.memory[c.vi+2] = (c.v[x] % 100) % 10
	case 0x55: // reg_dump(Vx,&I); Stores V0 to VX (including VX) in memory starting at address I. The offset from I is increased by 1 for each value written, but I itself is left unmodified.[d]
		var nx uint16
		for nx = 0; nx <= x; nx++ {
			c.memory[c.vi+nx] = c.v[nx]
		}
	case 0x65: // reg_load(Vx,&I); Fills V0 to VX (including VX) with values from memory starting at address I.
		// The offset from I is increased by 1 for each value written, but I itself is left unmodified.
		var nx uint16
		for nx = 0; nx <= x; nx++ {
			c.v[nx] = c.memory[c.vi+nx]
		}
	}
	c.pc += 2
	return nil
}

func (c *chip8) setKey(k int, state bool) {
	if state {
		c.key[k] = 1
	} else {
		c.key[k] = 0
	}
}

func (c *chip8) emulateCycle() error {
	var opcode uint16

	opcode = uint16(c.memory[c.pc])<<8 | uint16(c.memory[c.pc+1])

	handler, ok := c.opcodes[opcode&0xF000]
	if !ok {
		return fmt.Errorf("unknown opcode: 0x%X", opcode)
	}
	err := handler(opcode)

	if err != nil {
		return fmt.Errorf("unknown opcode: 0x%X", opcode)
	}
	return nil
}

func run(cpu *chip8) int {
	var err error
	var window *sdl.Window
	var renderer *sdl.Renderer

	err = sdl.Init(sdl.INIT_EVERYTHING)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to initialize sdl: %s\n", err)
		return 1
	}

	window, err = sdl.CreateWindow(windowTitle, sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
		windowWidth, windowHeight, sdl.WINDOW_SHOWN)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create window: %s\n", err)
		return 1
	}
	defer window.Destroy()

	renderer, err = sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create renderer: %s\n", err)
		return 1
	}
	defer renderer.Destroy()

	wavBuffer, wavSpec := sdl.LoadWAV("beep.wav")
	deviceID, err := sdl.OpenAudioDevice("", false, wavSpec, nil, 0)
	running := true

	for running {
		for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
			switch et := event.(type) {
			case *sdl.QuitEvent:
				running = false
			case *sdl.KeyboardEvent:
				if et.Type == sdl.KEYUP {
					cpu.setKey(cpu.keycodes[et.Keysym.Sym], false)
				} else if et.Type == sdl.KEYDOWN {
					cpu.setKey(cpu.keycodes[et.Keysym.Sym], true)
				}
			}
		}

		err := cpu.emulateCycle()

		if cpu.delayTimer > 0 {
			cpu.delayTimer--
		}

		if cpu.soundTimer > 0 {
			if cpu.soundTimer == 1 {
				go func() {
					_ = sdl.QueueAudio(deviceID, wavBuffer)
					sdl.PauseAudioDevice(deviceID, false)
				}()
			}
			cpu.soundTimer--
		}

		if err != nil {
			return 1
		}
		if cpu.drawFlag == 1 {
			renderer.SetDrawColor(0, 0, 0, 0x80)
			renderer.Clear()
			renderer.SetDrawColor(255, 255, 255, 0x80)
			for ny := 0; ny < 32; ny++ {
				for nx := 0; nx < 64; nx++ {
					if cpu.screen[nx][ny] == 1 {
						renderer.FillRect(&sdl.Rect{X: int32(nx) * 10, Y: int32(ny) * 10, W: 10, H: 10})
					}
				}
			}
			cpu.drawFlag = 0
			renderer.Present()
		}
		time.Sleep(time.Second / 360)
	}
	return 0
}

func main() {
	var err error
	if len(os.Args) != 2 {
		fmt.Println("Usage:", os.Args[0], "ROM_FILE")
		return
	}
	file := os.Args[1]

	cpu := chip8{}
	cpu.initialize()
	err = cpu.loadGame(file)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		return
	}
	run(&cpu)
}
