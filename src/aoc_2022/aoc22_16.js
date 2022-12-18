
function process_dat(enable_test_data = false) {
    const dat = enable_test_data ? test_dat() : raw_dat()
    dat1 = dat.map(process_dat_line)
    return dat1.reduce((a, v) => ({...a, ...v}), {})
}

function process_dat_line(d) {
    const [a, b] = d.split(';')
    const [valve, flow] = a.split('=')
    const tunnels = b.split(',')
    return ({
        [valve]: {
            valve,
            flow: parseInt(flow),
            is_open: false,
            tunnels,
        }
    })
}

const data = process_dat(true)

console.log(data)

function part_1() {
    // open valve if not already open and flow > 0
    // move
}

/*

0 (30):
@AA
BB 1+1 = 28 * 13 = 364
DD 1+1 = 28 * 20 = 560
HH 5+1 = 24 * 22 = 528
JJ 2+1 = 27 * 21 = 567 <<

3 (27): 567
@JJ
BB 3+1 = 23 * 13 = 299
DD 3+1 = 23 * 20 = 460 <<
HH 7+1 = 19 * 22 = 418

7 (23): 567 + 460
@DD
BB 2+1 = 20 * 13 = 260
HH 4+1 = 18 * 22 = 396 <<

12 (18): 567 + 460 + 396
BB 6+1 = 11 * 13 = 143

19 (11): 567 + 460 + 396 + 143
@BB
CC 1+1 = 9 * 2 = 18
EE 3+1 = 7 * 3 = 21 <<

23 (7): 567 + 460 + 396 + 143 + 21
@EE
CC 2+1 = 4 * 2 = 8
Last 2 steps subtotal = 29

OR

21 (9): 567 + 460 + 396 + 143 + 18
@CC
EE 2+1 = 6 * 3 = 18
Last 2 steps subtotal = 36

EE vs CC from BB shows that targetting highest single value is not necesarily optimal

This will also be shown (if I could be bothered to work it out) from the first step where JJ was highest value,
but (according to the provided answer) DD is optimal.


Hitlist of valves with a value
Target them in turn from current location

Keep track of:
* total_pressure
* valves_to_visit
* remaining time

pressure_earnt = (remaining_time - distance_to_valve - 1) * valve_pressure
total_pressure += pressure_earnt
remaining_time = remaining_time - distance_to_valve - 1
location = valve_location




    BB 13

AA      CC 2

    DD 20

        EE 3

            FF

                GG

                    HH 22


    II

        JJ 21

*/

function test_dat() {
    return `AA=0;DD,II,BB
BB=13;CC,AA
CC=2;DD,BB
DD=20;CC,AA,EE
EE=3;FF,DD
FF=0;EE,GG
GG=0;FF,HH
HH=22;GG
II=0;AA,JJ
JJ=21;II`
        .split('\n')
}

function raw_dat() {
    return `SW=0;LX,LD
VS=0;JO,OO
OO=10;KK,HD,VS,KI
DZ=8;KV,GX,WQ,BA,PK
GX=0;AA,DZ
IF=0;OI,DW
BO=0;UJ,ZT
KI=0;OO,KU
JT=3;FC,AM,KV,XP,XZ
TQ=0;AA,DW
KK=0;QW,OO
NR=0;UG,XM
VO=0;YR,AA
MS=17;LT,LX
JO=0;YR,VS
ZB=0;UJ,LT
ZT=0;XM,BO
YR=9;VO,FY,WB,JO
QS=0;QW,FY
UD=0;CA,JB
AP=0;CA,DW
KV=0;JT,DZ
JH=0;IK,UJ
LD=15;IK,SW
XK=0;XZ,BH
XM=11;XP,CJ,ZT,NR
FY=0;YR,QS
GI=22;TI
JB=14;WB,UD,WQ,HD
DW=6;AP,TQ,NQ,IF,PK
UJ=13;JH,ZB,BO
KU=0;CA,KI
WQ=0;JB,DZ
BA=0;BH,DZ
AA=0;YX,TQ,VO,GX,QP
TI=0;GI,UG
FC=0;QP,JT
CA=18;KU,UD,AP
QW=25;QS,KK
XZ=0;JT,XK
YX=0;AA,CJ
OI=0;IF,BH
NQ=0;AM,DW
QP=0;AA,FC
AM=0;NQ,JT
XP=0;XM,JT
BH=12;BA,XK,OI
HD=0;OO,JB
LT=0;MS,ZB
LX=0;MS,SW
CJ=0;XM,YX
PK=0;DW,DZ
IK=0;LD,JH
WB=0;YR,JB
UG=21;TI,NR`
        .split('\n')
}
