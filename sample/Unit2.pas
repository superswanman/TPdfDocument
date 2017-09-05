unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ExtCtrls, PdfDoc;

type
  TForm2 = class(TForm)
    MainMenu1: TMainMenu;
    F1: TMenuItem;
    O1: TMenuItem;
    N1: TMenuItem;
    A1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure O1Click(Sender: TObject);
    procedure A1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FPdf: TPdfDocument;
    FCurrentPage: Integer;
    procedure UpdatePage;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FPdf := TPdfDocument.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FPdf.Free;
end;

procedure TForm2.O1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute(Handle) then Exit;

  FPdf.LoadFromFile(OpenDialog1.FileName,
    procedure
    begin
      FCurrentPage := 0;
      UpdatePage;
    end);
end;

procedure TForm2.A1Click(Sender: TObject);
var
  fileName: string;
  fileType: TExportFileType;
begin
  if not FPdf.DocumentLoaded then Exit;
  if (FCurrentPage < 0) or (FCurrentPage >= FPdf.PageCount) then Exit;
  if not SaveDialog1.Execute(Handle) then Exit;

  case SaveDialog1.FilterIndex of
    1:
    begin
      fileName := ChangeFileExt(SaveDialog1.FileName, '.png');
      fileType := eftPng;
    end;
    2:
    begin
      fileName := ChangeFileExt(SaveDialog1.FileName, '.jpg');
      fileType := eftJpeg;
    end;
    else Exit;
  end;

  FPdf[FCurrentPage].ExportAsImage(fileName, fileType, 0, 0,
    procedure
    begin
      ShowMessage('現在のページを画像として保存しました');
    end);
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  if not FPdf.DocumentLoaded then Exit;
  if FCurrentPage = 0 then Exit;
  Dec(FCurrentPage);
  UpdatePage;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  if not FPdf.DocumentLoaded then Exit;
  if FCurrentPage >= FPdf.PageCount-1 then Exit;
  Inc(FCurrentPage);
  UpdatePage;
end;

procedure TForm2.UpdatePage;
var
  ms: TMemoryStream;
begin
  if not FPdf.DocumentLoaded then Exit;

  Button1.Enabled := False;
  Button2.Enabled := False;

  ms := TMemoryStream.Create;
  FPdf[FCurrentPage].ExportAsImage(ms, eftBmp, 0, 0,
    procedure
    begin
      try
        ms.Position := 0;
        Image1.Picture.Bitmap.LoadFromStream(ms);
      finally
        ms.Free;
      end;
      Label1.Caption := Format('%d / %d', [FCurrentPage+1, FPdf.PageCount]);
      Button1.Enabled := FCurrentPage > 0;
      Button2.Enabled := FCurrentPage < FPdf.PageCount-1;
    end);
end;

end.
