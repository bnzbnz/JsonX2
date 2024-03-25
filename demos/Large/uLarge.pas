unit uLarge;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  System.Generics.Collections, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, FMX.StdCtrls
  , RTTI
  , JsonX2
  , JsonX2.Types
  ;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowStats(AStats: IJX2Stats);
  end;

  FvalueConst = class(TJX2)
    applicableForLocalizedAspectName: Variant;
    applicableForLocalizedAspectValues: TJX2ValueList;
  end;

  FvalueConstraint = class(TJX2)
    localizedValue: TValue;
    [JX2AttrClass(FvalueConst)]
    valueConstraints: TJX2ObjList;
    applicableForLocalizedAspectName: TValue;
    applicableForLocalizedAspectValues: TJX2ValueList;
  end;

  TFaspectValues = class(TJX2)
    localizedValue: TValue;
    [JX2AttrClass(FvalueConstraint)]
    valueConstraints: TJX2ObjList;
  end;

  TaspectConstraint = class(TJX2)
    aspectDataType: TValue;
    itemToAspectCardinality: TValue;
    aspectMode: TValue;
    aspectRequired: TValue;
    aspectUsage: TValue;
    aspectEnabledForVariations: TValue;
    aspectApplicableTo: TJX2ValueList;
    aspectMaxLength: TValue;
    expectedRequiredByDate: TValue;
    aspectFormat: TValue;
  end;

  TcategoryAspectName = class(TJX2)
    categoryId: TValue;
    categoryName: TValue;
  end;

  TcategoryAspect = class(TJX2)
    localizedAspectName: TValue;
    aspectConstraint: TaspectConstraint;
    [JX2AttrClass(TFaspectValues)]
    aspectValues: TJX2ObjList;
  end;

  TcategoryAspects = class(TJX2)
    category: TcategoryAspectName;
    [JX2AttrClass(TcategoryAspect)]
    aspects: TJX2ObjList;
  end;

  // Object Container:

  TfetchItemAspectsContentType = class(TJX2)
  public
    categoryTreeId: TValue;
    categoryTreeVersion: TValue;
    [JX2AttrClass(TcategoryAspects)]
    categoryAspects: TJX2ObjList;
  end;

  // Interfaced Container:

  IfetchItemAspectsContentType = interface(IJX2)
    ['{46254129-B931-44AC-97FF-8972C865DFE7}']
    procedure SetcategoryTreeId(v: TValue);
    procedure SetcategoryTreeVersion(v: TValue);
    procedure SetcategoryAspects(v: TJX2ObjList);
    function GetcategoryTreeId: TValue;
    function GetcategoryTreeVersion: TValue;
    function GetcategoryAspects: TJX2ObjList;
    property categoryTreeId: TValue read GetcategoryTreeId write
    SetcategoryTreeId;
    property categoryTreeVersion: TValue read GetcategoryTreeVersion write
    SetcategoryTreeVersion;
    property categoryAspects: TJX2ObjList read GetcategoryAspects write
    SetcategoryAspects;
  end;

  TIfetchItemAspectsContentType = class(TIJX2, IJX2,
      IfetchItemAspectsContentType)
    [JX2AttrName('categoryTreeId')]
    FcategoryTreeId: TValue;
    [JX2AttrName('categoryTreeVersion')]
    FcategoryTreeVersion: TValue;
    [JX2AttrName('categoryAspects')]
    [JX2AttrClass(TcategoryAspects)]
    FcategoryAspects: TJX2ObjList;
    procedure SetcategoryTreeId(v: TValue);
    procedure SetcategoryTreeVersion(v: TValue);
    procedure SetcategoryAspects(v: TJX2ObjList);
    function GetcategoryTreeId: TValue;
    function GetcategoryTreeVersion: TValue;
    function GetcategoryAspects: TJX2ObjList;
  end;

var
  Form2: TForm2;

implementation
uses
  System.Diagnostics
  , JsonX2.Utils
  ;

{$R *.fmx}

procedure TForm2.ShowStats(AStats: IJX2Stats);
begin
  Memo1.Lines.Add('  Operations: ' + AStats.OpsCount.ToString);
  Memo1.Lines.Add(Format('  Duration: %dms: ', [AStats.DurationMS]));
  Memo1.Lines.Add('  Variants: ' + AStats.VariantCount.ToString);
  Memo1.Lines.Add('  TValue: ' + AStats.TValueCount.ToString);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  Obj: TfetchItemAspectsContentType;
  Json: string;
  LStats: IJX2Stats;
begin
  Memo1.Lines.Add('----> Object Container :');
  Obj := nil;
  LStats := nil;
  LStats := TJX2Stats.Create;
  try

    OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
    if not OpenDialog1.Execute then Exit;

    Memo1.Lines.Add('Filename : ' + OpenDialog1.Filename);
    Json := LoadStringFromFile(OpenDialog1.Filename, TEncoding.UTF8);

    Memo1.Lines.Add('Deserialization');
    Obj := JX2.Deserialize<TfetchItemAspectsContentType>(Json, [], LStats);
    ShowStats(LStats);

    Memo1.Lines.Add('Serialization');
    Json := JX2.Serialize(Obj, [], LStats);
    ShowStats(LStats);

  finally
    Obj.Free;
  end;
  Memo1.GoToTextEnd;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  IObj: IfetchItemAspectsContentType;
  Json: string;
  LStats: IJX2Stats;
begin
  Memo1.Lines.Add('----> Interfaced Container :');
  LStats := TJX2Stats.Create;

  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  if not OpenDialog1.Execute then Exit;

  Memo1.Lines.Add('Filename : ' + OpenDialog1.Filename);
  Json := LoadStringFromFile(OpenDialog1.Filename, TEncoding.UTF8);

  Memo1.Lines.Add('Deserialization');
  IObj := JX2.Deserialize<TIfetchItemAspectsContentType>(Json, [], LStats);
  ShowStats(LStats);

  Memo1.Lines.Add('Serialization');
  Json := JX2.Serialize(IObj, [], LStats);
  ShowStats(LStats);

  Memo1.GoToTextEnd;
end;

{$REGION 'Getter/Setter/Utils'}

procedure TIfetchItemAspectsContentType.SetcategoryTreeId(v: TValue); begin FcategoryTreeId := v; end;
procedure TIfetchItemAspectsContentType.SetcategoryTreeVersion(v: TValue); begin FcategoryTreeVersion := v; end;
procedure TIfetchItemAspectsContentType.SetcategoryAspects(v: TJX2ObjList); begin FcategoryAspects := v; end;
function TIfetchItemAspectsContentType.GetcategoryTreeId: TValue; begin Result := FcategoryTreeId; end;
function TIfetchItemAspectsContentType.GetcategoryTreeVersion: TValue; begin Result := FcategoryTreeVersion; end;
function TIfetchItemAspectsContentType.GetcategoryAspects: TJX2ObjList;begin Result := FcategoryAspects; end;

{$ENDREGION}

end.

