	// Floating point Verilog modules for CPE480
	// Created February 19, 2019 by Henry Dietz, http://aggregate.org/hankd
	// Distributed under CC BY 4.0, https://creativecommons.org/licenses/by/4.0/

	// Field definitions
	`define	WORD	[15:0]	// generic machine word size
	`define WORD_SIZE [15:0] //mistakenly used wrong word size var
	`define	INT	signed [15:0]	// integer size
	`define FLOAT	[15:0]	// half-precision float size
	`define FSIGN	[15]	// sign bit
	`define FEXP	[14:7]	// exponent
	`define FFRAC	[6:0]	// fractional part (leading 1 implied)

	// Constants
	`define	FZERO	16'b0	  // float 0
	`define F32767  16'h46ff  // closest approx to 32767, actually 32640
	`define F32768  16'hc700  // -32768
	`define NAN		16'hffc0

	// Count leading zeros, 16-bit (5-bit result) d=lead0s(s)
	module lead0s(d, s);
		output wire [4:0] d;
		input wire `WORD s;
		wire [4:0] t;
		wire [7:0] s8;
		wire [3:0] s4;
		wire [1:0] s2;
		assign t[4] = 0;
		assign {t[3],s8} = ((|s[15:8]) ? {1'b0,s[15:8]} : {1'b1,s[7:0]});
		assign {t[2],s4} = ((|s8[7:4]) ? {1'b0,s8[7:4]} : {1'b1,s8[3:0]});
		assign {t[1],s2} = ((|s4[3:2]) ? {1'b0,s4[3:2]} : {1'b1,s4[1:0]});
		assign t[0] = !s2[1];
		assign d = (s ? t : 16);
	endmodule

	// Float set-less-than, 16-bit (1-bit result) torf=a<b
	module fslt(torf, a, b);
		output wire torf;
		input wire `FLOAT a, b;
		assign torf = (a `FSIGN && !(b `FSIGN)) ||
			      (a `FSIGN && b `FSIGN && (a[14:0] > b[14:0])) ||
			      (!(a `FSIGN) && !(b `FSIGN) && (a[14:0] < b[14:0])) 
			      && ((a ^ `NAN) && !(b ^ `NAN));
	endmodule

	// Floating-point addition, 16-bit r=a+b
	module fadd(r, a, b);
		output wire `FLOAT r;
		input wire `FLOAT a, b;
		wire `FLOAT s;
		wire [8:0] sexp, sman, sfrac;
		wire [7:0] texp, taman, tbman;
		wire [4:0] slead;
		wire ssign, aegt, amgt, eqsgn;

		//temp
		wire `FLOAT temp;

		//NAN flag
		wire n;

		//NAN flag set
		assign n = ( a == `NAN || b == `NAN );


		//ouput based on NAN flag
		assign temp = ((a == 0) ? b : ((b == 0) ? a : s));
		assign r = (n ? `NAN : temp);

		assign aegt = (a `FEXP > b `FEXP);
		assign texp = (aegt ? (a `FEXP) : (b `FEXP));
		assign taman = (aegt ? {1'b1, (a `FFRAC)} : ({1'b1, (a `FFRAC)} >> (texp - a `FEXP)));
		assign tbman = (aegt ? ({1'b1, (b `FFRAC)} >> (texp - b `FEXP)) : {1'b1, (b `FFRAC)});
		assign eqsgn = (a `FSIGN == b `FSIGN);
		assign amgt = (taman > tbman);
		assign sman = (eqsgn ? (taman + tbman) : (amgt ? (taman - tbman) : (tbman - taman)));
		lead0s m0(slead, {sman, 7'b0});
		assign ssign = (amgt ? (a `FSIGN) : (b `FSIGN));
		assign sfrac = sman << slead;
		assign sexp = (texp + 1) - slead;
		assign s = (sman ? (sexp ? {ssign, sexp[7:0], sfrac[7:1]} : 0) : 0);
	endmodule

	// Floating-point multiply, 16-bit r=a*b
	module fmul(r, a, b);
		output wire `FLOAT r;
		input wire `FLOAT a, b;
		wire [15:0] m; // double the bits in a fraction, we need high bits
		wire [7:0] e;
		wire s;

		//temp wire
		wire `FLOAT temp;

		//NAN flag
		wire n;
		assign n = (a == `NAN || b == `NAN);

		assign s = (a `FSIGN ^ b `FSIGN);
		assign m = ({1'b1, (a `FFRAC)} * {1'b1, (b `FFRAC)});
		assign e = (((a `FEXP) + (b `FEXP)) -127 + m[15]);
		
		assign temp = (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]}));

		//output based on NAN flagA;
		assign r = (n ? `NAN : temp);
	endmodule

	// Floating-point reciprocal, 16-bit r=1.0/a
	// Note: requires initialized inverse fraction lookup table
	module frecip(r, a);
		output wire `FLOAT r;
		input wire `FLOAT a;

		//temp wire
		wire `FLOAT temp;

		//NAN flag
		wire n;

		reg [6:0] look[127:0];
		initial $readmemh0(look);

		//NAN flag set
		assign n = (a == `NAN || b == `NAN);

		assign temp `FSIGN = a `FSIGN;
		assign temp `FEXP = 253 + (!(a `FFRAC)) - a `FEXP;
		assign temp `FFRAC = look[a `FFRAC];

		//ouput based on NAN flag
		assign r = (n ? `NAN : temp);
	endmodule

	// Floating-point shift, 16 bit
	// Shift +left,-right by integer
	module fshift(r, f, i);
		output wire `FLOAT r;
		input wire `FLOAT f;
		input wire `INT i;
		
		//temp wire
		wire `FLOAT temp;

		//NAN flag
		wire n;
		//NAN flag set
		assign n = (a == `NAN || b == `NAN);

		assign temp `FFRAC = f `FFRAC;
		assign temp `FSIGN = f `FSIGN;
		assign temp `FEXP = (f ? (f `FEXP + i) : 0);

		//output based on NAN flag
		assign r = (n ? `NAN : temp);
	endmodule

	// Integer to float conversion, 16 bit
	module i2f(f, i);
		output wire `FLOAT f;
		input wire `INT i;

		//temp wire
		wire `FLOAT temp;

		//NAN flag
		wire n;
		//NAN flag set
		assign n = (a == `NAN || b == `NAN);

		wire [4:0] lead;
		wire `WORD pos;
		assign pos = (i[15] ? (-i) : i);
		lead0s m0(lead, pos);
		assign temp `FFRAC = (i ? ({pos, 8'b0} >> (16 - lead)) : 0);
		assign temp `FSIGN = i[15];
		assign temp `FEXP = (i ? (128 + (14 - lead)) : 0);

		//output based on NAN flag
		assign f = (n ? `NAN : temp);
	endmodule

	// Float to integer conversion, 16 bit
	// Note: out-of-range values go to -32768 or 32767
	module f2i(i, f);
		output wire `INT i;
		input wire `FLOAT f;
		wire `FLOAT ui;
		wire tiny, big;

		//temp wire
		wire `INT temp;

		//NAN flag
		wire n;
		//NAN flag set
		assign n = (a == `NAN || b == `NAN);

		fslt m0(tiny, f, `F32768);
		fslt m1(big, `F32767, f);
		assign ui = {1'b1, f `FFRAC, 16'b0} >> ((128+22) - f `FEXP);
		assign temp = (tiny ? 0 : (big ? 32767 : (f `FSIGN ? (-ui) : ui)));

		//output based on NAN flag
		assign i = (n ? `NAN : temp);
	endmodule

	// End of float library

	//ALU OPs
	`define ALUOP_SIZE  [3:0] // size of ALU op field
	`define ALUNOT      4'b0000
	`define ALUFLOAT    4'b0001
	`define ALUINT      4'b0010
	`define ALUNEG      4'b0011
	`define ALUNEGF     4'b0100
	`define ALURECIP    4'b0101
	`define ALUADD      4'b0110
	`define ALUMUL      4'b0111
	`define ALUSLT      4'b1000
	`define ALUAND      4'b1001
	`define ALUOR       4'b1010
	`define ALUSHIFT    4'b1011
	`define ALUXOR      4'b1100
	`define ALUADDF     4'b1101
	`define ALUMULF     4'b1110
	`define ALUSLTF     4'b1111

	// Integer and floating-point ALU
	module ALU (out, op, x, y);
		output `WORD_SIZE out;
		input `ALUOP_SIZE op;
		input `WORD_SIZE x, y;
		reg`WORD_SIZE temp;
		wire fsltout;
		wire `WORD_SIZE faddout, fmulout, frecipout, i2fout, f2iout;

		// instantiate floating point modules
		fslt myfslt(fsltout, x, y);
		fadd myfadd(faddout, x, y);
		fmul myfmul(fmulout, x, y);
		frecip myfrecip(frecipout, x);
		i2f myi2f(i2fout, x);
		f2i myf2i(f2iout, x);

		// assign temp to output
		assign out = temp;

		// assign output based on op
		always @* begin
			case (op)
				`ALUNOT: temp = ~x;
				`ALUFLOAT: temp = i2fout;
				`ALUINT: temp = f2iout;
				`ALUNEG: temp = -x;
				`ALUNEGF: temp = {~x`FSIGN,x[14:0]};
				`ALURECIP: temp = frecipout;
				`ALUADD: temp = x+y;
				`ALUMUL: temp = x*y;
				`ALUSLT: temp = x<y;
				`ALUAND: temp = x&y;
				`ALUOR: temp = x|y;
				`ALUSHIFT: temp = ((y < 32768) ? (x << y) : (x >> -y));
				`ALUXOR: temp = x^y;
				`ALUADDF: temp = faddout;
				`ALUMULF: temp = fmulout;
				`ALUSLTF: temp = fsltout;
			endcase
		end
	endmodule

	//Definitions
	// Format A field & values
	`define FA_FIELD        	[15]
	`define FA_FIELD_F0     	1
	`define FA_FIELD_F1to4  	0

	// Format B field & values
	`define FB_FIELD        	[14:13]
	`define FB_FIELD_F1     	1
	`define FB_FIELD_F2     	2
	`define FB_FIELD_F3     	3
	`define FB_FIELD_F4     	0

	// Format 0 Op codes
	`define F0_OP_FIELD_HIGH    [14:13]
	`define F0_OP_FIELD_LOW     [8]
	`define F0_OP_LEX           0
	`define F0_OP_LHI           1
	`define F0_OP_BRF           2
	`define F0_OP_BRT           3
	`define F0_OP_MEAS          4
	`define F0_OP_NEXT          5
	`define F0_OP_HAD           6

	// Format 1 Op codes
	`define F1_OPA_FIELD        [8]
	`define F1_OPA_FIELD_ALU    0
	`define F1_OPA_FIELD_OPB    1
	`define F1_OPB_FIELD        [7:4]
	`define F1_OPB_JUMPR        0
	`define F1_OPB_LOAD         8
	`define F1_OPB_STORE        9
	`define F1_OPB_COPY         10

	// Format 2 Op Codes
	`define F2_OP_FIELD         [12:8]
	`define F2_OP_ONE           0
	`define F2_OP_ZERO          1
	`define F2_OP_NOT           2

	// Format 3 Op Codes
	`define F3_OP_FIELD         [12:8]
	`define F3_OP_CCNOT         0
	`define F3_OP_CSWAP         1
	`define F3_OP_AND           2
	`define F3_OP_OP            3
	`define F3_OP_XOR           4
	`define F3_OP_SWAP          16
	`define F3_OP_CNOT          17

	// Define instruction operands
	`define IR_RD_FIELD         [12:9]
	`define IR_RS_FIELD         [3:0]
	`define IR_ALU_OP_FIELD     [7:4]
	`define IR_IMM8_FIELD       [7:0]
	`define IR_QAT_RA_FIELD     [7:0]
	`define IR2_QAT_RB_FIELD    [7:0]
	`define IR2_QAT_RC_FIELD    [15:8]

	// Define States
	`define STATE_SIZE  		[4:0]
	`define S_START     		0
	`define S_DECODE    		1
	`define S_SYS       		2
	`define S_QAT_1OP   		3
	`define S_QAT_MOP   		4
	`define S_ALU       		5
	`define S_JUMPR     		6
	`define S_LOAD      		7
	`define S_STORE     		8
	`define S_COPY      		9
	`define S_LEX       		10
	`define S_LHI       		11
	`define S_BRF       		12
	`define S_BRT       		13
	`define S_MEAS      		14
	`define S_NEXT      		15
	`define S_HAD       		16

	`define NOP             16'b0

	//array sizes
	`define IMEM_SIZE       	[2**16-1:0] // Instruction memory size 
	`define DMEM_SIZE       	[2**16-1:0] // Data memory size
	`define REGFILE_SIZE    	[2**4-1:0]  // Regfile size

	module processor(halt, reset, clk);
		output reg halt;
		input reset, clk;

		reg `WORD_SIZE instMem `IMEM_SIZE;  	//instruction memory
		reg `WORD_SIZE dataMem `DMEM_SIZE;  	//data memory
		reg `WORD_SIZE regfile `REGFILE_SIZE;   //register file

		reg `WORD_SIZE pc;                      //program counter register
		reg `WORD_SIZE tempPc;					//branchoff control
		reg `WORD_SIZE pc_old;					//old pc

		reg wait1;

		reg `WORD_SIZE ir1;						//instruction reg 1
		reg `WORD_SIZE ir2;						//instruction reg 2 (for QAT)

		reg `WORD_SIZE ir1_old;					//old instruction reg 1
		reg `WORD_SIZE ir2_old;					//old instruction reg 2 (for QAT)

		reg `WORD_SIZE qatA;					//qat reg a
		reg `WORD_SIZE qatB;					//qat reg b
		reg `WORD_SIZE qatC;					//qat reg c

		reg [3:0] s, d;							//operand regs

		reg pendpc; 							//pc pending flag
		reg [7:0] branchoff;					//branch value
		reg branch; 							//branch flag
		reg [15:0] jumpoff;						//jump value
		reg jump; 								//jump flag

		reg [7:0] imm8;							//8-bit immediate

		always @(reset) begin 
			halt = 0;
			pc = 0;
			ir1 = `NOP;
			ir2 = `NOP;
			branchoff = 0;

			$readmemh0(regfile); // register file
			$readmemh1(dataMem); //data file

		end

		function usesrd;
			input `WORD_SIZE inst;

			if(!(inst`FA_FIELD)) begin
				if(inst`FB_FIELD == `FB_FIELD_F1) begin
					if(inst`F1_OPA_FIELD == `F1_OPA_FIELD_ALU) begin
						usesrd = 1; 
					end else if (inst`F1_OPA_FIELD == `F1_OPA_FIELD_OPB) begin
						usesrd = ((inst`F1_OPB_FIELD == `F1_OPB_LOAD) || 
							(inst`F1_OPB_FIELD == `F1_OPB_COPY) || 
							(inst`F1_OPB_FIELD == `F1_OPB_STORE));
				end
			end 

			if(inst`FA_FIELD) begin
				usesrd = ({inst`F0_OP_FIELD_HIGH, inst`F0_OP_FIELD_LOW} == `F0_OP_LEX ||
					({inst`F0_OP_FIELD_HIGH, inst`F0_OP_FIELD_LOW} == `F0_OP_LHI));
				end
			end
		endfunction
		
		function setsrd;
			input `WORD_SIZE inst;

			if(!(inst`FA_FIELD)) begin
				if(inst`FB_FIELD == `FB_FIELD_F1) begin
					if(inst`F1_OPA_FIELD == `F1_OPA_FIELD_ALU) begin
						setsrd = 1; 
					end else if (inst`F1_OPA_FIELD == `F1_OPA_FIELD_OPB) begin
						setsrd = ((inst`F1_OPB_FIELD == `F1_OPB_LOAD) || 
							(inst`F1_OPB_FIELD == `F1_OPB_COPY));
				end
			end 

			if(inst`FA_FIELD) begin
				setsrd = ({inst`F0_OP_FIELD_HIGH, inst`F0_OP_FIELD_LOW} == `F0_OP_LEX ||
					({inst`F0_OP_FIELD_HIGH, inst`F0_OP_FIELD_LOW} == `F0_OP_LHI));
				end
			end

		endfunction

		//Stage 0 - get instruction from instruction memory
		always @(posedge clk) begin
			//branch / jump
			tempPc = (branch ? branchoff : (jump ? jumpoff : pc));

			//if not blocked by rd race condition
			if(wait1) begin
				pc <= tempPc;
			end else begin
				//get instruction
				ir1 = instMem[tempPc];

				//if QAT, get ir1, ir2
				if(!(ir1`FA_FIELD)) begin
					if(ir1`FB_FIELD == `FB_FIELD_F2 || ir1`FB_FIELD == `FB_FIELD_F3) begin
						//increment pc to next part of QAT instruction
						pc = tempPc + 1;

						//second QAT inst in ir2
						ir2 = instMem[pc];
					end
				//if not QAT, get ir1
				end 

				//if pc race condition
				if(pendpc) begin
					ir1 <= `NOP;
					pc <= tempPc;
				end else begin
					pc <= tempPc + 1;
				end
			
				pc_old <= pc;
			end
		end
		
		//Stage 1 - get regs from regfile based on instruction
		always @(posedge clk) begin
			//if no race instruction
			if((ir1 != `NOP) &&
				(setsrd(ir1_old) && usesrd(ir1)) && 
				(ir1 `IR_RD_FIELD == ir1_old`IR_RD_FIELD)) begin
				
				wait1 = 1;
				ir1_old = `NOP;
			
			end else begin
				wait1 = 0;
				//assign reg operands s, d
				s <= ir1`IR_RS_FIELD;
				d <= ir1`IR_RD_FIELD;

				//assign qat operands a, b, c
				qatA <= ir1`IR_QAT_RA_FIELD;
				qatB <= ir2`IR2_QAT_RB_FIELD;
				qatC <= ir2`IR2_QAT_RC_FIELD;

				imm8 <= ir1`IR_IMM8_FIELD;	

				ir1_old <= ir1;
			end
		end

		//stage 2 pieces and parts
		reg `WORD_SIZE result; 

		wire [15:0] aluOut;
		wire [3:0] aluOp;
		assign aluOp = ir1 `IR_ALU_OP_FIELD;

		//wiring aluOut, aluOP, s, and d into ALU instantiation
		ALU alu(.out(aluOut), .op(aluOp), .x(regfile[s]), .y(regfile[d]));

		//stage 2 - ALU
		always @(posedge clk) begin

			case(ir1 `FA_FIELD)
				//if format 0
				`FA_FIELD_F0:
					case({ir1`F0_OP_FIELD_HIGH, ir1`F0_OP_FIELD_LOW})

						`F0_OP_LEX: begin result <= {imm8[7] ? 8'hFF : 8'h00, imm8}; end
						`F0_OP_LHI: begin result <= {imm8, regfile[d][7:0]}; end
						`F0_OP_BRF: begin result <= pc + imm8; end
						`F0_OP_BRT: begin result <= pc + imm8; end
						`F0_OP_MEAS: begin halt <= 1; end
						`F0_OP_NEXT: begin halt <= 1; end
						`F0_OP_HAD: begin halt <= 1; end

					endcase

				//if format 1-4
				`FA_FIELD_F1to4:
					case(ir1`FB_FIELD)
						//if format 1 - ALU + JUMPR/LOAD/STR/CPY
						`FB_FIELD_F1:
							case (ir1`F1_OPA_FIELD)
								//S_ALU
								`F1_OPA_FIELD_ALU: begin result <= aluOut; end

								//JUMPR/LOAD/STR/CPY
								`F1_OPA_FIELD_OPB: 
									case (ir1`F1_OPB_FIELD)
										`F1_OPB_JUMPR: begin result <= regfile[d]; end
										`F1_OPB_LOAD: begin result <= dataMem[regfile[s]]; end
										`F1_OPB_STORE: begin dataMem[regfile[s]] <= regfile[d]; end
										`F1_OPB_COPY: begin result <= regfile[s]; end
									endcase
							endcase

						//S_QAT_1OP
						`FB_FIELD_F2: begin halt <= 1; end

						//S_QAT_MOP
						`FB_FIELD_F3: begin halt <= 1; end
					
						//SYS
						`FB_FIELD_F4: begin halt <= 1; end
					endcase
			endcase

			//stage 3 - latch result to register d if needed
			if(setsrd(ir1)) begin
				//if brf/brt
				if(ir1`FA_FIELD) begin
					if(({ir1`F0_OP_FIELD_HIGH, ir1`F0_OP_FIELD_LOW} == `F0_OP_BRF) || ({ir1`F0_OP_FIELD_HIGH, ir1`F0_OP_FIELD_LOW} == `F0_OP_BRT)) begin
							pendpc <= 1;
							branch <= 1; 
							branchoff <= result;
					end
				//if not jump
				end else if(!(ir1`FA_FIELD)) begin
					if(ir1`FB_FIELD == `FB_FIELD_F1) begin
						if(ir1`F1_OPA_FIELD == `F1_OPA_FIELD_OPB) begin
							if(ir1`F1_OPB_FIELD == `F1_OPB_JUMPR) begin
								pendpc <= 1;
								jump = 1;
								jumpoff <= result; 
							end
						end
					end
				end else begin
					//else, latch to rd
					regfile[d] <= result;
					jump <= 0;
					branch <= 0;
				end
			
			end else begin
		
				jump <= 0;
				branch <= 0;

			end
		
		end
	endmodule
