unit UpdateFirmware;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  ButtonPanel, StdCtrls;

type

  { TFirmWareForm }

  TFirmWareForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    lsBoxFolders: TListBox;
    procedure lsBoxFoldersClick(Sender: TObject);
  private

  public
    FirmWareSelected: string;
    procedure ReadFolder(aFolder: string);

  end;
// Type FirmWareFile = ( bootcode.bin, start.elf,fixup.dat
var
  FirmWareForm: TFirmWareForm;

implementation

{$R *.lfm}

{ TFirmWareForm }

procedure TFirmWareForm.lsBoxFoldersClick(Sender: TObject);
begin
  if lsBoxFolders.SelCount > 0 then
  begin
    FirmWareSelected := lsBoxFolders.Items[lsBoxFolders.ItemIndex];
    ButtonPanel1.OKButton.Enabled := True;
  end
  else
  begin
    ButtonPanel1.OKButton.Enabled := False;
  end;
end;

procedure TFirmWareForm.ReadFolder(aFolder: string);
var
  Directory, Mask: string;
  sr: TSearchRec;
begin
  Directory := aFolder + PathDelim;
  Mask := '*.*';
  if FindFirst(Directory + Mask, faAnyFile, sr) = 0 then
    repeat
      if (sr.Name = 'RPi') or (sr.Name = 'RPi2') or (sr.Name = 'RPi3') then

      begin
        lsBoxFolders.Items.Add(sr.Name);
      end;
    until FindNext(sr) <> 0;
  FindClose(sr);
end;


end.
