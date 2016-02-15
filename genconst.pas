unit GenConst;

{$mode objfpc}{$H+}

interface

uses SerConF;

type
  tDS345Command = (
    AECL = longint(TRG) + 1, AMPL, ATTL, FREQ, FSMP, FUNC, INVT, OFFS, PCLR, PHSE,
    BCNT, DPTH, FDEV, MDWF, MENA, MKSP, MRKF, MTYP, PDEV, RATE, SPAN, SPCF, SPFR, SPMK, STFR, TRAT, TSRC,
    AMRT, AMOD, LDWF,
    DENA, STAT
              );

  tDS335Command = (
    KEYS = longint(STAT) + 1,  SYNC, TERM,
    FSEN, SDIR, SWEN
                    );
const

  DS345Command: array [longint(RST)..longint(STAT)] of ansistring = (
    '*RST', '*IDN', '*RCL', '*SAV', '*TST', '*CAL', '*CLS', '*STB', '*SRE', '*ESR', '*ESE', '*PSC' , '*TRG',
    'AECL', 'AMPL', 'ATTL', 'FREQ', 'FSMP', 'FUNC', 'INVT', 'OFFS', 'PCLR', 'PHSE',
    'BCNT', 'DPTH', 'FDEV', 'MDWF', 'MENA', 'MKSP', 'MRKF', 'MTYP', 'PDEV', 'RATE', 'SPAN', 'SPCF', 'SPFR', 'SPMK', 'STFR', 'TRAT', 'TSRC',
    'AMRT', 'AMOD', 'LDWF',
    'DENA', 'STAT'
    );

  DS335Command: array [longint(RST)..longint(SWEN)] of ansistring = (
    '*RST', '*IDN', '*RCL', '*SAV', '*TST', '', '*CLS', '*STB', '*SRE', '*ESR', '*ESE', '*PSC' , '*TRG',
    'AECL', 'AMPL', 'ATTL', 'FREQ', '', 'FUNC', 'INVT', 'OFFS', '', '',
    '', '', '', '', '', '', '', 'STYP', '', '', '', '', 'SPFR', '', 'STFR', 'SRAT', 'TSRC',
    '', '', '',
    'DENA', 'STAT',
    'KEYS',  'SYNC', 'TERM',
    'FSEN', 'SDIR', 'SWEN'
     );

  DS345WaveForm: array [0..5] of ansistring = (
    'Синусоида',
    'Квадратная',
    'Треугольная',
    'Пилообразная',
    'Шум',
    'Пользовательская'
         );

  DS335WaveForm: array [0..4] of ansistring = (
    'Синусоида',
    'Квадратная',
    'Треугольная',
    'Пилообразная',
    'Шум'
         );


implementation

end.

