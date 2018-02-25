unit Main;

 { Updater is a small software to automate the process of update
 your Ultibo programs into a raspberry PI

  Copyright (C) 2018 Jorge Turiel jorge.turiel@gmail.com

  This code is release under Creative Common Attribution-ShareAlike 4.0 licence

}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Dialogs, ExtCtrls, StdCtrls, EditBtn,
  XMLPropStorage, ComCtrls, ActnList, IdTelnet, IdIPWatch, IdGlobal,
  IdComponent, IdStack, Classes, FileUtil;

type

  { TUpdaterFrm }

  TUpdaterFrm = class(TForm)
    acForceIpServer: TAction;
    acConnect: TAction;
    acUpdateCmdLine: TAction;
    acUpdate: TAction;
    ActionList1: TActionList;
    btUpdate: TButton;
    btUpdateServer: TButton;
    btIpWebServer: TButton;
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
    mnMessages: TMemo;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    XMLPropStorage1: TXMLPropStorage;
    procedure acConnectExecute(Sender: TObject);
    procedure acForceIpServerExecute(Sender: TObject);
    procedure acUpdateCmdLineExecute(Sender: TObject);
    procedure acUpdateExecute(Sender: TObject);
    procedure btUpdateServerClick(Sender: TObject);
    procedure DirServerChange(Sender: TObject);
    procedure edHostChange(Sender: TObject);
    procedure edWebServerIpDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IdTelnet1Connected(Sender: TObject);
    procedure IdTelnet1DataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure IdTelnet1Disconnected(Sender: TObject);
    procedure IdTelnet1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure KernelNameChange(Sender: TObject);
  private

    procedure SendData(aString: string);
    procedure UpdateCmdLine(aPath: string);
    procedure UpdateButtons;
  public

  end;

var
  UpdaterFrm: TUpdaterFrm;

implementation

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

procedure TUpdaterFrm.acForceIpServerExecute(Sender: TObject);
begin
  SendData('update set server ' + edWebServerIp.Text);
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
  SendData('update get all /r');
end;

procedure TUpdaterFrm.btUpdateServerClick(Sender: TObject);
begin
  SendData('update set server ' + edWebServerIp.Text);

end;

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
  btUpdate.Enabled := IdTelnet1.Connected;
  btUpdateServer.Enabled := IdTelnet1.Connected;
  btIpWebServer.Enabled := IdTelnet1.Connected;
  if IdTelnet1.Connected then
  begin
    btConnect.Caption := 'Disconnect';
  end
  else
  begin
    btConnect.Caption := 'Connect';
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
begin
  IdTelnet1.Host := edHost.Text;
  UpdateButtons;
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
