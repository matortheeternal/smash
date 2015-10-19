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

  TRttiIni = class (TObject)
  private
    class function ReadValue(section: string; var ini: TMemIniFile;
      field: TRttiField): TValue;
    class procedure WriteValue(section: string; var ini: TMemIniFile;
      field: TRttiField; aValue: TValue);
    class function GetIniAttribute(Obj: TRttiObject): IniSectionAttribute;
  public
    class procedure Load(filename: string; obj: TObject);
    class procedure Save(filename: string; obj: TObject);
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
  for Attr in Obj.GetAttributes do begin
    if Attr is IniSectionAttribute then
      exit(IniSectionAttribute(Attr));
  end;
  result := nil;
end;

class procedure TRttiIni.Load(filename: string; obj: TObject);
var
  ctx: TRttiContext;
  objType: TRttiType;
  Field: TRttiField;
  IniSection: IniSectionAttribute;
  Ini: TMemIniFile;
  CurrentSection: string;
  value: TValue;
begin
  ctx := TRttiContext.Create;
  try
    Ini := TMemIniFile.Create(FileName);
    try
      objType := ctx.GetType(Obj.ClassInfo);
      for Field in objType.GetFields do begin
        IniSection := GetIniAttribute(Field);
        if Assigned(IniSection) then
          CurrentSection := IniSection.Section;
        value := ReadValue(CurrentSection, ini, Field);
        if not value.IsEmpty then
           field.SetValue(obj, value);
      end;
    finally
      Ini.Free;
    end;
  finally
    ctx.Free;
  end;
end;

class function TRttiIni.ReadValue(section: string; var ini: TMemIniFile;
  field: TRttiField): TValue;
var
  fieldType: string;
begin
  Result := TValue.Empty;
  fieldType := field.FieldType.Name;

  // exit if value doesn't exist in ini being loaded
  // this allows us to use default values from the object's constructor
  if not ini.ValueExists(section, field.Name) then
    exit;

  // load string, Integer, and Boolean fields from ini
  if fieldType = 'string' then
    Result := TValue.From(ini.ReadString(section, field.Name, ''))
  else if fieldType = 'Integer' then
    Result := TValue.From(ini.ReadInteger(section, field.Name, 0))
  else if fieldType = 'Int64' then
    Result := TValue.From(ini.ReadInteger(section, field.Name, 0))
  else if fieldType = 'TDateTime' then
    Result := TValue.From(ini.ReadFloat(section, field.Name, 0))
  else if fieldType = 'Boolean' then
    Result := TValue.From(ini.ReadBool(section, field.Name, false));
end;

class procedure TRttiIni.WriteValue(section: string; var ini: TMemIniFile;
  field: TRttiField; aValue: TValue);
var
  fieldType: string;
begin
  fieldType := field.FieldType.Name;
  if fieldType = 'string' then
    ini.WriteString(section, field.Name, aValue.AsString)
  else if fieldType = 'Integer' then
    ini.WriteInteger(section, field.Name, aValue.AsInteger)
  else if fieldType = 'Int64' then
    ini.WriteInteger(section, field.Name, aValue.AsInt64)
  else if fieldType = 'TDateTime' then
    ini.WriteFloat(section, field.Name, aValue.AsType<TDateTime>)
  else if fieldType = 'Boolean' then
    ini.WriteBool(section, field.Name, aValue.AsBoolean)
end;

class procedure TRttiIni.Save(filename: string; obj: TObject);
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
    ini := TMemIniFile.Create(FileName);
    try
      objType := ctx.GetType(Obj.ClassInfo);
      for field in objType.GetFields do begin
        IniSection := GetIniAttribute(Field);
        if Assigned(IniSection) then
          CurrentSection := IniSection.Section;
        WriteValue(CurrentSection, ini, field, Field.GetValue(obj));
      end;
    finally
      Ini.UpdateFile;
      Ini.Free;
    end;
  finally
    ctx.Free;
  end;
end;

end.
