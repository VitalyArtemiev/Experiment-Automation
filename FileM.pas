unit FileM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  tFileModule = class
    FileResult: integer;

    function FullCfgDir: string; inline;
    function SaveParams(FileName: ansistring): integer;
    function LoadParams(FileName: ansistring): integer;
    function SaveConfig(FileName: ansistring): integer;
    function LoadConfig(FileName: ansistring): integer;
  end;

var
  FileMgr: tFileModule;

implementation

end.

