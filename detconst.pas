unit DetConst;

{$mode objfpc}{$H+}

interface

uses SerConF;


const
{  SR844Command: array[longint(RST)..longint(LIAE)] of ansistring = (
    '*RST', '*IDN', '', '', '', '', '*CLS', '*STB', '*SRE', '*ESR', '*ESE', '*PSC' , 'TRIG',
    'PRST', 'LOCL', 'OVRM',
    'FMOD', 'HARM', 'FREQ', 'FRAQ', 'FRIQ', 'PHAS', 'APHS', 'REFZ', 'WRSV', 'AWRS', 'INPZ',
    'SENS', 'AGAN', 'CRSV', 'ACRS', 'OFLT', 'OFSL', 'SETL',
    'DDEF', 'DRAT', 'FPOP', 'DOFF', 'AOFF', 'DEXP',
    'AUXI', 'AUXO',
    'OUTX', 'KCLK', 'ALRM', 'SSET', 'RSET', 'KNOB', 'KEYP',
    'SSTR', 'SFIN', 'SSTP', 'SMOD', 'RSTO', 'RRDY', 'RCLR', 'RMOD',
    'SRAT', 'SEND', 'TSTR', 'STRT', 'PAUS', 'REST',
    'OUTP', 'OUTR', 'SNAP', 'SPTS', 'TRCA', 'TRCB', 'TRCL', 'FAST', 'STRD',
    'ERRS', 'ERRE', 'LIAS', 'LIAE'
                                                 );

  SR830Command: array[longint(RST)..longint(ARSV)] of ansistring = (
    '*RST', '*IDN', '', '', '', '', '*CLS', '*STB', '*SRE', '*ESR', '*ESE', '*PSC' , 'TRIG',
    '', 'LOCL', 'OVRM',
    'FMOD', 'HARM', 'FREQ', '', '', 'PHAS', 'APHS', '', '', '', '',
    'SENS', 'AGAN', '', '', 'OFLT', 'OFSL', '',
    'DDEF', '', 'FPOP', '', 'AOFF', '',
    'OAUX', 'AUXV',
    'OUTX', 'KCLK', 'ALRM', 'SSET', 'RSET', '', '',
    '', '', '', '', '', '', '', 'RMOD',
    'SRAT', 'SEND', 'TSTR', 'STRT', 'PAUS', 'REST',
    'OUTP', 'OUTR', 'SNAP', 'SPTS', 'TRCA', 'TRCB', 'TRCL', 'FAST', 'STRD',
    'ERRS', 'ERRE', 'LIAS', 'LIAE',
    'RSLP', 'SLVL', 'ISRC', 'IGND', 'ICPL', 'ILIN', 'SYNC', 'OEXP', 'ARSV'
                                              );      }

  SR844TransferParams: array [0..9] of ansistring = (
    'X',
    'Y',
    'R (V)',
    'R (dBm)',
    'Φ',
    'AUX IN 1',
    'AUX IN 2',
    'Опорная частота',
    'Дисплей 1',
    'Дисплей 2'
               );

  SR844TimeConstants: array [0..17] of ansistring = (
    '100 мкс',
    '300 мкс',
    '1 мс',
    '3 мс',
    '10 мс',
    '30 мс',
    '100 мс',
    '300 мс',
    '1 с',
    '3 с',
    '10 с',
    '30 с',
    '100 с',
    '300 с',
    '1 кс',
    '3 кс',
    '10 кс',
    '30 кс'
           );

  SR844Sensitivities: array [0..14] of ansistring = (
    '100 нВсркв/-127 дБ',
    '300 нВсркв/-117 дБ',
    '1 мкВсркв/-107 дБ',
    '3 мкВсркв/-97 дБ',
    '10 мкВсркв/-87 дБ',
    '30 мкВсркв/-77 дБ',
    '100 мкВсркв/-67 дБ',
    '300 мкВсркв/-57 дБ',
    '1 мВсркв/-47 дБ',
    '3 мВсркв/-37 дБ',
    '10 мВсркв/-27 дБ',
    '30 мВсркв/-17 дБ',
    '100 мВсркв/-7 дБ',
    '300 мВсркв/+3 дБ',
    '1 Всркв/+13 дБ'
         );

  SR830TransferParams: array [0..10] of ansistring = (
    'X',
    'Y',
    'R',
    'Φ',
    'AUX IN 1',
    'AUX IN 2',
    'AUX IN 3',
    'AUX IN 4',
    'Опорная частота',
    'Дисплей 1',
    'Дисплей 2'
               );

  SR830TimeConstants: array [0..19] of ansistring = (
    '10 мкс',
    '30 мкс',
    '100 мкс',
    '300 мкс',
    '1 мс',
    '3 мс',
    '10 мс',
    '30 мс',
    '100 мс',
    '300 мс',
    '1 с',
    '3 с',
    '10 с',
    '30 с',
    '100 с',
    '300 с',
    '1 кс',
    '3 кс',
    '10 кс',
    '30 кс'
           );

  SR830Sensitivities: array [0..26] of ansistring = (
    '2 нВ/фА',
    '5 нВ/фА',
    '10 нВ/фА',
    '20 нВ/фА',
    '50 нВ/фА',
    '100 нВ/фА',
    '200 нВ/фА',
    '500 нВ/фА',
    '1 мкВ/пА',
    '2 мкВ/пА',
    '5 мкВ/пА',
    '10 мкВ/пА',
    '20 мкВ/пА',
    '50 мкВ/пА',
    '100 мкВ/пА',
    '200 мкВ/пА',
    '500 мкВ/пА',
    '1 мВ/нА',
    '2 мВ/нА',
    '5 мВ/нА',
    '10 мВ/нА',
    '20 мВ/нА',
    '50 мВ/нА',
    '100 мВ/нА',
    '200 мВ/нА',
    '500 мВ/нА',
    '1 В/мкА'
             );

  SR844CH1Params: array [0..4] of ansistring = (
    'X',
    'R (V)',
    'R (dBm)',
    'X Noise',
    'AUX IN 1'
              );

  SR844CH2Params: array [0..4] of ansistring = (
    'Y',
    'Φ',
    'Y Noise (V)',
    'Y Noise (dBm)',
    'AUX IN 2'
              );

  SR830CH1Params: array [0..4] of ansistring = (
    'X',
    'R',
    'X Noise',
    'AUX IN 1',
    'AUX IN 2'
              );

  SR830CH2Params: array [0..4] of ansistring = (
    'Y',
    'Φ',
    'Y Noise',
    'AUX IN 3',
    'AUX IN 4'
              );
  implementation

end.

