unit RttiIni;

interface

uses
  SysUtils, Classes, Rtti, TypInfo, IniFiles;

type
  IniSectionAttribute = class(TCustomAttribute)
  private
    FSection: string;
  public
    constructor Create(const aSection: String);
    property Section: string read FSection write FSection;
  end;

  TRttiIni = class(TObject)
  private
    class function ReadValue(Section: string; var ini: TMemIniFile;
      field: TRttiField): TValue;
    class procedure WriteValue(Section: string; var ini: TMemIniFile;
      field: TRttiField; aValue: TValue);
    class function GetIniAttribute(Obj: TRttiObject): IniSectionAttribute;
  public
    class procedure Load(filename: string; Obj: TObject);
    class procedure Save(filename: string; Obj: TObject);
  end;

implementation

{ TIniSection }
constructor IniSectionAttribute.Create(const aSection: String);
begin
  FSection := aSection;
end;

{ TIniPersist }
class function TRttiIni.GetIniAttribute(Obj: TRttiObject): IniSectionAttribute;
var
  Attr: TCustomAttribute;
begin
  for Attr in Obj.GetAttributes do
  begin
    if Attr is IniSectionAttribute then
      exit(IniSectionAttribute(Attr));
  end;
  result := nil;
end;

class procedure TRttiIni.Load(filename: string; Obj: TObject);
var
  ctx: TRttiContext;
  objType: TRttiType;
  field: TRttiField;
  IniSection: IniSectionAttribute;
  ini: TMemIniFile;
  CurrentSection: string;
  value: TValue;
begin
  ctx := TRttiContext.Create;
  try
    ini := TMemIniFile.Create(filename);
    try
      objType := ctx.GetType(Obj.ClassInfo);
      for field in objType.GetFields do
      begin
        IniSection := GetIniAttribute(field);
        if Assigned(IniSection) then
          CurrentSection := IniSection.Section;
        value := ReadValue(CurrentSection, ini, field);
        if not value.IsEmpty then
          field.SetValue(Obj, value);
      end;
    finally
      ini.Free;
    end;
  finally
    ctx.Free;
  end;
end;

class function TRttiIni.ReadValue(Section: string; var ini: TMemIniFile;
  field: TRttiField): TValue;
var
  fieldType: string;
begin
  result := TValue.Empty;
  fieldType := field.fieldType.Name;

  // exit if value doesn't exist in ini being loaded
  // this allows us to use default values from the object's constructor
  if not ini.ValueExists(Section, field.Name) then
    exit;

  // load string, Integer, and Boolean fields from ini
  if fieldType = 'string' then
    result := TValue.From(ini.ReadString(Section, field.Name, ''))
  else if fieldType = 'Integer' then
    result := TValue.From(ini.ReadInteger(Section, field.Name, 0))
  else if fieldType = 'Int64' then
    result := TValue.From(ini.ReadInteger(Section, field.Name, 0))
  else if fieldType = 'TDateTime' then
    result := TValue.From(ini.ReadFloat(Section, field.Name, 0))
  else if fieldType = 'Boolean' then
    result := TValue.From(ini.ReadBool(Section, field.Name, false));
end;

class procedure TRttiIni.WriteValue(Section: string; var ini: TMemIniFile;
  field: TRttiField; aValue: TValue);
var
  fieldType: string;
begin
  fieldType := field.fieldType.Name;
  if fieldType = 'string' then
    ini.WriteString(Section, field.Name, aValue.AsString)
  else if fieldType = 'Integer' then
    ini.WriteInteger(Section, field.Name, aValue.AsInteger)
  else if fieldType = 'Int64' then
    ini.WriteInteger(Section, field.Name, aValue.AsInt64)
  else if fieldType = 'TDateTime' then
    ini.WriteFloat(Section, field.Name, aValue.AsType<TDateTime>)
  else if fieldType = 'Boolean' then
    ini.WriteBool(Section, field.Name, aValue.AsBoolean)
end;

class procedure TRttiIni.Save(filename: string; Obj: TObject);
var
  ctx: TRttiContext;
  objType: TRttiType;
  field: TRttiField;
  IniSection: IniSectionAttribute;
  ini: TMemIniFile;
  CurrentSection: string;
begin
  ctx := TRttiContext.Create;
  try
    ini := TMemIniFile.Create(filename);
    try
      objType := ctx.GetType(Obj.ClassInfo);
      for field in objType.GetFields do
      begin
        IniSection := GetIniAttribute(field);
        if Assigned(IniSection) then
          CurrentSection := IniSection.Section;
        WriteValue(CurrentSection, ini, field, field.GetValue(Obj));
      end;
    finally
      ini.UpdateFile;
      ini.Free;
    end;
  finally
    ctx.Free;
  end;
end;

end.
