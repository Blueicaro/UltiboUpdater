unit Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, ExtCtrls;

type

  { TOptionsFrm }

  TOptionsFrm = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkGeneral: TCheckGroup;
    DirectoryFirmware: TDirectoryEdit;
    GroupBox1: TGroupBox;
  private

  public

  end;

var
  OptionsFrm: TOptionsFrm;

implementation

{$R *.lfm}

end.

