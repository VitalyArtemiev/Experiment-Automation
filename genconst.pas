unit GenConst;

{$mode objfpc}{$H+}

interface

uses SerConF;

const
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

