unit Main;

 { Updater is a small software to automate the process of update
 your Ultibo programs into a raspberry PI

  Copyright (C) 2018 Jorge Turiel jorge.turiel@gmail.com

  This code is release under Creative Common Attribution-ShareAlike 4.0 licence
  v0.4
       -.Added more funcions.
       -.Added Kernelupdate. Now you can update firmware files. Ultibo now accepts
                           send kernel files using telnet. Now you can selected
                           the kernel folder and will be copy to web server, then
                           a update kernel command will be send to your pi with ultibo.
       -.Added Reboot. Reboot your pi
       -.Added Type Any command. You can type any command to your ultibo
       -.Remove update cmdline with the ip server, because the cmdline file has can have more entries,
         and this entries has to be in one line, so this functions must be rewrite, and for the moment
         was removed. In next releases will be back (I hope).

  v0.3 Add comandline -run.
       -run : will connect to your Ultibo/Pi and send a update comand. Using
              ipwebserve, ip of raspberry etc, of last sesion
       -Kernel You can pass the kernel file as parameter.

  v0.2 First stable realease

}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Dialogs, ExtCtrls, StdCtrls, EditBtn,
  XMLPropStorage, ComCtrls, ActnList, Menus, IdTelnet, IdIPWatch, IdGlobal,
  IdComponent, IdStack, Classes, FileUtil, PropertyStorage;

type

  { TUpdaterFrm }

  TUpdaterFrm = class(TForm)
    acForceIpServer: TAction;
    acConnect: TAction;
    acLoadProfile: TAction;
    acSaveProfile: TAction;
    acSendFile: TAction;
    acReboot: TAction;
    acAnyCommand: TAction;
    acExit: TAction;
    acFirmwareUpdate: TAction;
    acOptions: TAction;
    acUpdateCmdLine: TAction;
    acUpdate: TAction;
    ActionList1: TActionList;
    btUpdate: TButton;
    btUpdateServer: TButton;
    btConnect: TButton;
    DirServer: TDirectoryEdit;
    edHost: TEdit;
    edWebServerIp: TEdit;
    IdIPWatch1: TIdIPWatch;
    KernelName: TFileNameEdit;
    IdTelnet1: TIdTelnet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    mnComandos: TMenuItem;
    mnMessages: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    XMLPropStorage1: TXMLPropStorage;
    procedure acAnyCommandExecute(Sender: TObject);
    procedure acConnectExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acForceIpServerExecute(Sender: TObject);
    procedure acFirmwareUpdateExecute(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure acRebootExecute(Sender: TObject);
    procedure acSendFileExecute(Sender: TObject);
    procedure acUpdateCmdLineExecute(Sender: TObject);
    procedure acUpdateExecute(Sender: TObject);
    procedure btUpdateServerClick(Sender: TObject);
    procedure DirServerChange(Sender: TObject);
    procedure edHostChange(Sender: TObject);
    procedure edWebServerIpDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdTelnet1Connected(Sender: TObject);
    procedure IdTelnet1DataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure IdTelnet1Disconnected(Sender: TObject);
    procedure IdTelnet1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure KernelNameChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure mnMessagesChange(Sender: TObject);
    procedure XMLPropStorage1StoredValues0Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure XMLPropStorage1StoredValues0Save(Sender: TStoredValue;
      var Value: TStoredType);
  private
    AutoUpate: boolean;
    firstShow: boolean;
    ParamKernel: string;
    FirmWareFolder: string;
    DoNotReboot: boolean;
    Comand: string;  //ULtimo comando enviado desdel TypeAnyCommand
    procedure Params(aParam: string);
    procedure SendData(aString: string);
    procedure UpdateCmdLine(aPath: string);
    procedure UpdateButtons;
  public

  end;

var
  UpdaterFrm: TUpdaterFrm;

implementation

uses Options, UpdateFirmware;

{$R *.lfm}

{ TUpdaterFrm }

procedure TUpdaterFrm.acConnectExecute(Sender: TObject);
begin

  IdTelnet1.Host := edHost.Text;
  try
    try
      if not IdTelnet1.Connected then
      begin
        mnMessages.Lines.Clear;
        IdTelnet1.Connect;
      end
      else
        IdTelnet1.Disconnect(False);
    except
      on E: EIdSocketError do
      begin
        IdTelnet1.Disconnect(False);
        ShowMessage('Fail: ' + E.Message);
        StatusBar1.Panels[0].Text := E.Message;
        IdTelnet1.Disconnect(False);
      end;
    end;
  finally
    UpdateButtons;
  end;
end;

procedure TUpdaterFrm.acAnyCommandExecute(Sender: TObject);
begin
  if InputQuery('Send a comand', 'Type a comand.', Comand) then
  begin
    SendData(Comand);
  end;
end;

procedure TUpdaterFrm.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TUpdaterFrm.acForceIpServerExecute(Sender: TObject);
begin
  SendData('update set server ' + edWebServerIp.Text);
end;

procedure TUpdaterFrm.acFirmwareUpdateExecute(Sender: TObject);
const
  Ficheros: array [1..3] of string = ('bootcode.bin', 'start.elf', 'fixup.dat');
var
  F: TFirmWareForm;
  Origen: string;
  Destino: TCaption;
  Copiar: boolean;
  Fallo: boolean;
  I: integer;
begin
  //Copiar archivos de la localización por defecto
  F := TFirmWareForm.Create(self);
  try
    F.ReadFolder(FirmWareFolder);
    F.ShowModal;
    //Copiar al directorio del servidor.
    Fallo := False;
    for I := 1 to Length(Ficheros) do
    begin
      Origen := FirmWareFolder + PathDelim + F.FirmWareSelected +
        PathDelim + Ficheros[I];
      Destino := DirServer.Text + PathDelim + Ficheros[I];
      Copiar := CopyFile(origen, Destino, [cffOverwriteFile]);
      if Copiar = False then
      begin
        Fallo := True;
        ShowMessage('I can not copy file: ' + Ficheros[I]);
      end;
    end;
    if not Fallo then
    begin
      // SendData('UPDATE CHECK FIRMWARE');
    end;
  finally
    FreeAndNil(F);
  end;

end;

procedure TUpdaterFrm.acOptionsExecute(Sender: TObject);
var
  F: TOptionsFrm;
begin
  F := TOptionsFrm.Create(Self);
  try
    F.DirectoryFirmware.Text := FirmWareFolder;
    F.chkGeneral.Checked[0] := DoNotReboot;
    if F.ShowModal = mrOk then
    begin
      FirmWareFolder := F.DirectoryFirmware.Text;
      DoNotReboot := F.chkGeneral.Checked[0];
    end;
  finally
    FreeAndNil(F);
  end;
end;

procedure TUpdaterFrm.acRebootExecute(Sender: TObject);
begin
  SendData('reboot');
end;

procedure TUpdaterFrm.acSendFileExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin

  end;
end;

procedure TUpdaterFrm.acUpdateCmdLineExecute(Sender: TObject);
begin
  UpdateCmdLine(DirServer.Text);
end;

procedure TUpdaterFrm.acUpdateExecute(Sender: TObject);
var
  Nombre: RawByteString;
  Destino: TCaption;
  Copiar: boolean;
begin
  Nombre := ExtractFileName(KernelName.Text);
  Destino := DirServer.Text + PathDelim + Nombre;
  Copiar := CopyFile(KernelName.Text, Destino, [cffOverwriteFile]);
  if not Copiar then
  begin
    mnMessages.Lines.Add('Can not copy file!');
    Exit;
  end;
  if DoNotReboot then
  begin
    SendData('update get kernel');
  end
  else
  begin
    SendData('update get kernel /r');
  end;
end;

procedure TUpdaterFrm.btUpdateServerClick(Sender: TObject);
begin
  SendData('update set server ' + edWebServerIp.Text);

end;

{El fichero cmdline puede contener más entradas. Y deben ir seguidas en las misma
línea, por eso hay que rehacer esta opción. Pospuesta hasta la próxima versión}
procedure TUpdaterFrm.UpdateCmdLine(aPath: string);
var
  aString: string;
  lsCmd: TStringList;
  I: integer;
begin
  if FileExists(DirServer.Text + PathDelim + 'cmdline.txt') then
  begin
    aString := 'There is a copy of cmdline.txt file on your webserver' +
      LineEnding + 'Do you want update it with the ip adreess of your webserver?';
    if MessageDlg('Update Ip', aString, mtInformation, mbYesNo, 0, mbNo) = mrYes then
    begin
      lsCmd := TStringList.Create;
      try
        lsCmd.LoadFromFile(DirServer.Text + PathDelim + 'cmdline.txt');
        I := 0;
        while I < lsCmd.Count do
        begin
          if Pos('SHELL_UPDATE_HTTP_SERVER', lsCmd[I]) = 1 then
          begin
            lsCmd[I] := 'SHELL_UPDATE_HTTP_SERVER=' + edWebServerIp.Text;
            lsCmd.SaveToFile(DirServer.Text + PathDelim + 'cmdline.txt');
            I := lsCmd.Count;
          end;
          I := I + 1;
        end;
      finally
        FreeAndNil(lsCmd);
      end;
    end;
  end;
end;

procedure TUpdaterFrm.UpdateButtons;
begin
  acForceIpServer.Enabled := IdTelnet1.Connected;
  acUpdate.Enabled := IdTelnet1.Connected;
  acUpdateCmdLine.Enabled := IdTelnet1.Connected;
  acSendFile.Enabled := IdTelnet1.Connected;
  acReboot.Enabled := IdTelnet1.Connected;
  acAnyCommand.Enabled := IdTelnet1.Connected;
  acFirmwareUpdate.Enabled := IdTelnet1.Connected;
  if IdTelnet1.Connected then
  begin
    acConnect.Caption := 'Disconnect';
    //btConnect.Caption := 'Disconnect';
  end
  else
  begin
    acConnect.Caption := 'Connect';
    //   btConnect.Caption := 'Connect';
  end;
end;

procedure TUpdaterFrm.DirServerChange(Sender: TObject);
begin
  DirServer.Hint := DirServer.Text;
end;

procedure TUpdaterFrm.edHostChange(Sender: TObject);
begin
  edHost.Hint := edHost.Text;
  IdTelnet1.Host := edHost.Text;
end;

procedure TUpdaterFrm.edWebServerIpDblClick(Sender: TObject);
begin
  edWebServerIp.Text := IdIPWatch1.CurrentIP;
end;

procedure TUpdaterFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    try
      if IdTelnet1.Connected then
      begin
        IdTelnet1.Disconnect(False);
      end;
    except
      on E: EIdSocketError do
      begin
        CloseAction := caFree;
      end;
    end;
  finally
    CloseAction := caFree;
  end;
end;

procedure TUpdaterFrm.FormCreate(Sender: TObject);
var
  I: integer;
begin
  IdTelnet1.Host := edHost.Text;
  UpdateButtons;
  firstShow := True;
  AutoUpate := False;
  ParamKernel := '';
  if ParamCount >= 1 then
  begin
    for I := 1 to ParamCount do
    begin
      Params(ParamStr(I));
    end;
  end;
end;

procedure TUpdaterFrm.FormDestroy(Sender: TObject);
begin

end;

procedure TUpdaterFrm.FormShow(Sender: TObject);
begin
  if firstShow = True then
  begin
    firstShow := False;
    if AutoUpate then
    begin
      acConnect.Execute;
      mnMessages.Lines.Add(
        'Trying update automatic. Reason: you used -run as comand param');
      acUpdate.Execute;
    end;
    if ParamKernel <> '' then
    begin
      KernelName.Text := ParamKernel;
    end;
  end;
end;

procedure TUpdaterFrm.IdTelnet1Connected(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TUpdaterFrm.IdTelnet1DataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
var
  I: integer;
  Cadena: string;
begin
  for I := 0 to Length(Buffer) - 1 do
  begin
    if (Buffer[I] = 13) or (Buffer[I] = 10) or (Buffer[I] in [32..255]) then
    begin
      Cadena := Cadena + char(Buffer[I]);
    end;
  end;
  mnMessages.Lines.AddText(Cadena);
  mnMessages.SelStart := Length(mnMessages.Text);

end;

procedure TUpdaterFrm.IdTelnet1Disconnected(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TUpdaterFrm.IdTelnet1Status(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);

begin
  StatusBar1.Panels[0].Text := AStatusText;
end;

procedure TUpdaterFrm.KernelNameChange(Sender: TObject);
begin
  KernelName.Hint := KernelName.Text;
end;

procedure TUpdaterFrm.MenuItem1Click(Sender: TObject);
begin

end;

procedure TUpdaterFrm.mnMessagesChange(Sender: TObject);
begin

end;

procedure TUpdaterFrm.XMLPropStorage1StoredValues0Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  FirmWareFolder := Value;
end;

procedure TUpdaterFrm.XMLPropStorage1StoredValues0Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := FirmWareFolder;
end;

procedure TUpdaterFrm.Params(aParam: string);
begin
  if aParam = '-run' then
  begin
    AutoUpate := True;
    Exit;
  end;
  if Pos('-Kernel:', aParam) = 1 then
  begin
    ParamKernel := Copy(aParam, 9, Length(aParam));
  end;
end;

procedure TUpdaterFrm.SendData(aString: string);
var
  I: integer;
  Cadena: string;
begin
  try
    Cadena := aString + #10 + #13;
    try
      for I := 1 to Length(Cadena) do
      begin
        IdTelnet1.SendCh(Cadena[I]);
      end;
    except
      on E: EIdSocketError do
      begin
        ShowMessage('Fail: ' + E.Message);
        StatusBar1.Panels[0].Text := E.Message;
        IdTelnet1.Disconnect(False);
      end;
    end;
  finally
    UpdateButtons;
  end;
end;

end.
