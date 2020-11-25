/*
* Authors: Travis Bowen, Judah Voth, Ben Luckett, Will Shapiro
* Date: 11/8/2020
* Assignment: Tangled Pipeline
* Code was borrowed from Dr.Dietz previous tangled solution from the previous assignment and PINKY from old assignments and changed to fit with the Tangled instruction set.
* Testing was done through Iverilog
*/
// basic sizes of things
`define DATA	[15:0]
`define ADDR	[15:0]
`define SIZE	[65535:0]
`define INST	[15:0]
`define OP	[15:8]
`define IORR	[8]
`define RB	[11:8]
`define RD	[7:4]
`define RN	[3:0]
`define REGS    [15:0]
`define OPT     [15:8]
`define	OPR	[11:8]	
`define	OPA	[7:0]	// 8-bit immed or a
`define	WORD	[15:0]	// generic machine word size
`define	INT	signed [15:0]	// integer size
`define FLOAT	[15:0]	// half-precision float size
`define FSIGN	[15]	// sign bit
`define FEXP	[14:7]	// exponent
`define FFRAC	[6:0]	// fractional part (leading 1 implied)
`define	FZERO	16'b0	  // float 0
`define F32767  16'h46ff  // closest approx to 32767, actually 32640
`define F32768  16'hc700  // -32768
`define OP4		[15:12]

// Tangled OPcodes, also states
`define	TOPsys		8'h00 //Passed
`define	TOPjumpr	8'h10 //Passed
`define	TOPneg		8'h11 //Passed
`define	TOPnegf		8'h12 //Passed
`define	TOPnot		8'h13 //Passed
`define	TOPbrf		8'h20 //Passed
`define	TOPbrt		8'h30 //Passed
`define	TOPlex		8'h40 //Passed
`define	TOPlhi		8'h50 //Passed
`define	TOPadd		8'h60 //Passed
`define	TOPmul		8'h61 //Passed
`define	TOPslt		8'h62 //Passed
`define	TOPand		8'h63 //Passed
`define	TOPor		8'h64 //Passed
`define	TOPxor		8'h65 //Passed
`define	TOPshift	8'h66 //Passed
`define	TOPaddf		8'h70 //Passed
`define	TOPmulf		8'h71 //Passed
`define	TOPsltf		8'h72 //Passed
`define	TOPrecip	8'h74 //Passed
`define	TOPfloat	8'h78 //Passed
`define	TOPint		8'h79 //Passed
`define	TOPcopy		8'h80 //Passed
`define	TOPload		8'h81 //Passed
`define	TOPstore	8'h82 //Passed

// Qat OPcodes, also states
`define	QOPnot		8'h90
`define	QOPone		8'h91
`define	QOPzero		8'h92
`define	QOPhad		8'ha0
`define	QOPcnot		8'hb0
`define	QOPswap		8'hb1
`define	QOPccnot	8'hc0
`define	QOPcswap	8'hc1
`define	QOPand		8'hc3
`define	QOPor		8'hc4
`define	QOPxor		8'hc5
`define	QOPmeas		8'he0
`define	QOPnext		8'hf0

// make NOP (after fetch) an unconditional PRE 0
`define NOP             16'h1400 //

//Shorthand
`define	DONE	state <= `Start; }
`define	HALT	halt <= 1; }
`define	QAT1	`HALT
`define	QAT2	qargs <= i[pc]; pc <= pc + 1; `HALT
`define	SEX8TO16(V)	{ {8{V[7]}}, V[7:0] }

module processor(halt, reset, clk);
  output reg halt;
  input reset, clk;

  reg `DATA r `REGS;	// register file
  reg `ADDR sltcheck; //For set less than
  reg `DATA d `SIZE;	// data memory
  reg `INST i `SIZE;	// instruction memory
  reg `ADDR pc;		// program counter
  reg `ADDR tpc, pc0, pc1;
  reg `INST ir;		// instruction register
  reg `INST ir0, ir1, ir2;
  reg `DATA im0, rd1, rn1, res, rd2;
  reg pcadder;
  reg `INST qargs;	
  reg `ADDR target;	// jump target
  reg jump;		// are we jumping?
  reg zreg;		// z flag
  wire pendz;		// z update pending?
  wire pendpc;		// pc update pending?
  reg wait1;		// need to stall in stage 1?
  reg [11:0] prefix;	// 12-bit prefix value
  reg havepre;		// is prefix valid?

  initial $readmemh0(r); // register file
  initial $readmemh1(i); // instruction memory

  always @(reset) {
    halt = 0;
    pc = 0;
    ir0 = `NOP;
    ir1 = `NOP;
    ir2 = `NOP;
    jump = 0;
    havepre = 0;
  }

  function setsrdT;
    input `INST inst;
   
    setsrdT = ((inst `OPT == `TOPneg) ||
    (inst `OPT == `TOPnegf) ||
    (inst `OPT == `TOPnot) ||
    (inst `OPT == `TOPadd) ||
    (inst `OPT == `TOPmul) ||
    (inst `OPT == `TOPslt) ||
    (inst `OPT == `TOPand) ||
    (inst `OPT == `TOPor) ||
    (inst `OPT == `TOPxor) ||
    (inst `OPT == `TOPshift) ||
    (inst `OPT == `TOPaddf) ||
    (inst `OPT == `TOPmulf) ||
    (inst `OPT == `TOPsltf) ||
    (inst `OPT == `TOPrecip) ||
    (inst `OPT == `TOPfloat) ||
    (inst `OPT == `TOPint) ||
    (inst `OPT == `TOPcopy) ||
    (inst `OPT == `TOPload));
  endfunction

  function setsrd4T;
    input `INST inst;
    
    setsrd4T = ((inst `OP4 == 4'h4) || (inst `OP4 == 4'h5));
  endfunction


  function setspcT;
    input `INST inst;
    
    setspcT = ((inst `OPT == `TOPjumpr) ||
    (inst `OPT == `TOPbrf) ||
    (inst `OPT == `TOPbrt));
  endfunction

  function coded4;
    input `INST inst;
    
    coded4 = ((inst `OP4 == 4'h4)||
    (inst `OP4 == 4'h5)||
    (inst `OP4 == 4'ha)||
    (inst `OP4 == 4'he)||
    (inst `OP4 == 4'hf));
  endfunction

  function usesimT;
    input `INST inst;
   
    usesimT = ((inst `OPT == `TOPlex) || (inst `OPT == `TOPlhi));
  endfunction


  function usesrdT;
    input `INST inst;
    usesrdT = ((((inst `OPT >= `TOPjumpr) && (inst `OPT < `TOPbrf)) || ((inst `OPT > `TOPlhi) && (inst `OPT <= `TOPstore))) && (inst != `NOP));
  endfunction

  function isBranchT;
    input `INST inst;
    isBranchT = ((inst `OP4 == 4'h2) || (inst `OP4 == 4'h3));
  endfunction

  function isJump;
    input `INST inst;

    isJump = (ir1 `OP == `TOPjumpr);
  endfunction

  function interlock;
    //ir2 = stage 3, ir1 = stage 2
    input `INST ir0, ir1, ir2;
    interlock = ((ir0 != `NOP) &&
    ((setsrdT(ir1) && ((usesrdT(ir0) && (ir0 `RD == ir1 `RD)) || (usesrnT(ir0) && (ir0 `RN == ir1 `RD)))) || 
    (setsrdT(ir2) && ((usesrdT(ir0) && (ir0 `RD == ir2 `RD)) || (usesrnT(ir0) && (ir0 `RN == ir2 `RD)))) ||
    (setsrd4T(ir1) && ((usesrdT(ir0) && (ir0 `RD == ir1 `RB)) || (usesrnT(ir0) && (ir0 `RN == ir1 `RB)))) ||
    (setsrd4T(ir2) && ((usesrdT(ir0) && (ir0 `RD == ir2 `RB)) || (usesrnT(ir0) && (ir0 `RN == ir2 `RB)))) ||
    (setsrd4T(ir1) && ((isBranchT(ir0) && (ir0 `RB == ir1 `RB)))) ||
    (setsrd4T(ir2) && ((isBranchT(ir0) && (ir0 `RB == ir2 `RB)))) ||
    (setsrdT(ir1) && ((isBranchT(ir0) && (ir0 `RB == ir1 `RD)))) ||
    (setsrdT(ir2) && ((isBranchT(ir0) && (ir0 `RB == ir2 `RD)))) ));
  endfunction


  function usesrnT;
    input `INST inst;
    usesrnT = ((inst `OPT == `TOPadd) ||
    (inst `OPT == `TOPmul) ||
    (inst `OPT == `TOPslt) ||
    (inst `OPT == `TOPand) ||
    (inst `OPT == `TOPor) ||
    (inst `OPT == `TOPxor) ||
    (inst `OPT == `TOPshift) ||
    (inst `OPT == `TOPaddf) ||
    (inst `OPT == `TOPmulf) ||
    (inst `OPT == `TOPsltf) ||
    (inst `OPT == `TOPcopy) ||
    (inst `OPT == `TOPload) || 
    (inst `OPT == `TOPbrt) ||
    (inst `OPT == `TOPbrf) ||
    (inst `OPT == `TOPstore));
  endfunction

  // pending PC update?
  assign pendpc = (setspcT(ir0) || setspcT(ir1) || setspcT(ir2));

  // stage 0: instruction fetch and immediate extend
  always @(posedge clk) {
    tpc = (jump ? target : pc);

    if (wait1) {
      ir = i[tpc];
      if (interlock(ir0,ir1,ir2)) {
        //do nothing
      } else {
        pc <= tpc + 1;

        if(isBranchT(ir0)) {
          //do nothing
        } else {
          ir0 <= ir;
        }
      }
    } else {
      // not blocked by stage 1
      ir = i[tpc];
      if (0) { //pendpc
        // waiting... pc doesn't change
        $display("pc pending");

        ir0 <= `NOP;
        pc <= tpc;
      } else {

        //HAVEPRE NEVER SET TO 1
        if (usesimT(ir)) {
          // extend immediate
          im0 <= {{12{ir[3]}}, ir `RN};
          //im0 <= {(havepre ? prefix : {12{ir[3]}}), ir `RN};
          havepre <= 0;
        }

        //IF INTERLOCK
        if (interlock(ir0,ir1,ir2)) { 
          // stall waiting for register value
          pcadder = 0;
        } else {
          //IF BRANCH OR JUMP
          if(isBranchT(ir0) || isJump(ir1)) {
            //do nothing
          } else {
            ir0 <= ir;
          }
          pcadder = 1;
        }
      }

        pc <= tpc + pcadder;
      }

      pc0 <= tpc;
    }
    
    $display("0 %h", ir);
  }

  // stage 1: register read
  always @(posedge clk) {
    $display("1 %h", ir0);

    if (interlock(ir0,ir1,ir2)) { 
      // stall waiting for register value
      wait1 = 1;
      ir1 <= `NOP;
    } else {
      // all good, get operands (even if not needed)
      wait1 = 0;
      rd1 <= ((ir0 `RD == 15) ? pc0 : r[ir0 `RD]);
      rn1 <= (usesimT(ir0) ? im0 : ((ir0 `RN == 15) ? pc0 : r[ir0 `RN]));
      
      //why?
      if(ir0 `OP4 == 4'h2) {
        if (r[ir0 `OPR] == 0) {
          target <= pc + {{8{ir0[7]}}, ir0[7:0]} - 1;
          jump <= 1;
          ir0 <= `NOP;
        } else {
           jump <= 0;
           ir0 <= ir;
           wait1 = 0;
        }	

        ir1 <= `NOP;

      //why?
      } else if(ir0 `OP4 == 4'h3) {
        if (r[ir0 `OPR] != 0) {
          target <= pc + {{8{ir0[7]}}, ir0[7:0]} - 1;
          jump <= 1;
          ir0 <= `NOP;
        } else {
          jump <= 0;
          ir0 <= ir;
          wait1 = 0;
        }

        ir1 <= `NOP;

      } else if(isJump(ir1)) {
        //do nothing
      } else {
        ir1 <= ir0;
        jump <= 0;
      }

    }
  }

  wire `DATA faddr, fmulr, frecipr, i2fr, f2ir;
  wire fsltr;

  fadd myfadd(faddr, rd1, rn1);
  fmul myfmul(fmulr, rd1, rn1);
  fslt myfslt(fsltr, rd1, rn1);
  frecip myfrecip(frecipr, rd1);
  i2f myi2f(i2fr, rd1);
  f2i myf2i(f2ir, rd1);

  // stage 2: ALU, data memory access
  always @(posedge clk) {
  $display("2 %h", ir1);
  if (ir1 == `NOP) {
  //do nothing
  } else {
    // let the instruction execute
    if (coded4(ir1)) {
      case (ir1 `OP4)
        4'h4:	{ res <= {{8{ir1[7]}}, ir1[7:0]}; }
        4'h5:	{ res <= { ir1 `OPA, r[ir1 `OPR][7:0] }; }
        4'ha:	{ halt <= 1; }
        4'he: 	{ halt <= 1; }
        4'hf:	{ halt <= 1; }
        default: halt <= 1;
      endcase
    } else {
      case (ir1 `OP)
        `TOPsys:	{ `HALT }
        `TOPjumpr:	{ target <= rd1; jump <= 1; ir0 <= `NOP; ir1 <= `NOP; ir2 <= `NOP;}
        `TOPneg:	{ res <= -rd1; }
        `TOPnegf:	{ res <= 16'h8000 ^ rd1; }
        `TOPnot:	{ res <= ~rd1; }
        `TOPlex:	{ res <= {{8{ir1[7]}}, ir1[7:0]}; }
        `TOPlhi:	{ res <= { ir `OPA, r[ir `OPR][7:0] }; }
        `TOPadd:	{ res <= rd1 + rn1; }
        `TOPmul:	{ res <= rd1 * rn1; }
        `TOPslt:	{ sltcheck = (rd1 - rn1); res <=  (sltcheck[15] ? 16'b1 : 16'b0); }
        //((rd1 < rn1) ? 16'h0001 : 16'h0000); }
        `TOPand:	{ res <= rd1 & rn1; }
        `TOPor:		{ res <= rd1 | rn1; }
        `TOPxor:	{ res <= rd1 ^ rn1; }
        `TOPshift:	{ res <= ((rn1[15] == 0) ? (rd1 << rn1) : (rd1 >> -rn1)); }
        `TOPaddf:	{ res <= faddr; }
        `TOPmulf:	{ res <= fmulr; }
        `TOPsltf:	{ res <= { 15'h00, fsltr }; }
        `TOPrecip:	{ res <= frecipr; }
        `TOPfloat:	{ res <= i2fr; }
        `TOPint:	{ res <= f2ir; }
        `TOPcopy:	{ res <= rn1; }
        `TOPload:	{ res <= d[rn1]; }
        `TOPstore:	{ d[rn1] <= rd1; }
        `QOPnot:	{ `QAT1; }
        `QOPone:	{ `QAT1; }
        `QOPzero:	{ `QAT1; }
        `QOPhad:	{ `QAT1; }
        `QOPcnot:	{ `QAT2; }
        `QOPswap:	{ `QAT2; }
        `QOPccnot:	{ `QAT2; }
        `QOPcswap:	{ `QAT2; }
        `QOPand:	{ `QAT2; }
        `QOPor:		{ `QAT2; }
        `QOPxor:	{ `QAT2; }
        `QOPmeas:	{ `QAT1; }
        `QOPnext:	{ `QAT1; }
        default: { halt <= 1; } // make it stop
      endcase
    }

  }
  
  if(ir1 `OP != `TOPjumpr) {
    ir2 <= ir1;
  }
  
  rd2 <= rd1;
  }


  // stage 3: store in register
  always @(posedge clk) {
    $display("3 %h",  ir2);
    // put result in rd if we should
    if (setsrdT(ir2)) { // setsrd4T(ir2) Needs to check for LHI and LEX 
      r[ir2 `RD] <= res;
    } 
    if (setsrd4T(ir2)) {
      if(ir2 `OP4 == 4'h4) {
        r[ir2 `RB] <= res;
      } else {
        r[ir2 `RB][15:8] <= res[15:8];
      }
    }
  }


endmodule

module testbench;
reg reset = 0;
reg clk = 0;
wire halted;
processor PE(halted, reset, clk);
initial {
$dumpfile;
$dumpvars(0, PE.ir);
#10 reset = 1;
#10 reset = 0;
while (!halted) {
#10 clk = 1;
#10 clk = 0;
}
$finish;
}
endmodule

//NAN Library
`define NAN    16'hffc0
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
/*assign torf = (a `FSIGN && !(b `FSIGN)) ||
(a `FSIGN && b `FSIGN && (a[14:0] > b[14:0])) ||
(!(a `FSIGN) && !(b `FSIGN) && (a[14:0] < b[14:0]));*/
assign torf = ((a == `NAN)||(b == `NAN) ? 1'b0 : (a `FSIGN && !(b `FSIGN)) ||
(a `FSIGN && b `FSIGN && (a[14:0] > b[14:0])) ||
(!(a `FSIGN) && !(b `FSIGN) && (a[14:0] < b[14:0])));

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
assign r = ((a == 0) ? b : ((b == 0) ? a : s));
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
assign r = ((a == 0) ? b : ((b == 0) ? a : ((a == `NAN)||(b == `NAN) ? `NAN : s)));
endmodule

// Floating-point multiply, 16-bit r=a*b
module fmul(r, a, b);
output wire `FLOAT r;
input wire `FLOAT a, b;
wire [15:0] m; // double the bits in a fraction, we need high bits
wire [7:0] e;
wire s;
assign s = (a `FSIGN ^ b `FSIGN);
assign m = ({1'b1, (a `FFRAC)} * {1'b1, (b `FFRAC)});
assign e = (((a `FEXP) + (b `FEXP)) -127 + m[15]);
//assign r = (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]}));
assign r = ((a == `NAN)||(b == `NAN) ? `NAN : (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]})));
endmodule

// Floating-point reciprocal, 16-bit r=1.0/a
// Note: requires initialized inverse fraction lookup table
module frecip(r, a);
wire `WORD temp;
output wire `FLOAT r;
input wire `FLOAT a;
reg [6:0] look[127:0];
initial $readmemh3(look);
assign temp `FSIGN = a `FSIGN;
assign temp `FEXP = 253 + (!(a `FFRAC)) - a `FEXP;
assign temp `FFRAC = look[a `FFRAC];
assign r = (~|a) ? `NAN : temp;
endmodule

// Floating-point shift, 16 bit
// Shift +left,-right by integer
module fshift(r, f, i);
output wire `FLOAT r;
input wire `FLOAT f;
input wire `INT i;
assign r `FFRAC = f `FFRAC;
assign r `FSIGN = f `FSIGN;
assign r `FEXP = (f ? (f `FEXP + i) : 0);
assign r = ((f==`NAN)||(i==`NAN)) ? `NAN : r;
endmodule

// Integer to float conversion, 16 bit
module i2f(f, i);
output wire `FLOAT f;
input wire `INT i;
wire [4:0] lead;
wire `WORD pos;
assign pos = (i[15] ? (-i) : i);
lead0s m0(lead, pos);
assign f `FFRAC = (i ? ({pos, 8'b0} >> (16 - lead)) : 0);
assign f `FSIGN = i[15];
assign f `FEXP = (i ? (128 + (14 - lead)) : 0);
endmodule

// Float to integer conversion, 16 bit
// Note: out-of-range values go to -32768 or 32767
module f2i(i, f);
output wire `INT i;
input wire `FLOAT f;
wire `FLOAT ui;
wire tiny, big;
fslt m0(tiny, f, `F32768);
fslt m1(big, `F32767, f);
assign ui = {1'b1, f `FFRAC, 16'b0} >> ((128+22) - f `FEXP);
//assign i = (tiny ? 0 : (big ? 32767 : (f `FSIGN ? (-ui) : ui)));
assign i = ((f == `NAN) ? 16'h8000 : (tiny ? 0 : (big ? 32767 : (f `FSIGN ? (-ui) : ui))));
endmodule