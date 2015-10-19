unit RttiTranslation;

interface

uses
  SysUtils, Classes, StdCtrls, ComCtrls, Buttons, Menus, Rtti, TypInfo;

type
  FormPrefixAttribute = class(TCustomAttribute)
  private
    FPrefix: string;
  public
    constructor Create(const aPrefix: String);
    property Prefix: string read FPrefix write FPrefix;
  end;

  FormSectionAttribute = class(TCustomAttribute)
  private
    FSection: string;
  public
    constructor Create(const aSection: String);
    property Section: string read FSection write FSection;
  end;

  TRttiTranslation = class (TObject)
  private
    class function ReadValue(section: string; var sl: TStringList;
      field: TRttiField; subfield: string): string;
    class procedure WriteValue(section: string; value: string; var sl: TStringList;
      field: TRttiField; subfield: string);
    class function GetPrefixAttribute(Obj: TRttiObject): FormPrefixAttribute;
    class function GetSectionAttribute(Obj: TRttiObject): FormSectionAttribute;
  public
    class procedure Load(filename: string; obj: TObject); overload;
    class procedure Load(var sl: TStringList; obj: TObject); overload;
    class procedure Save(filename: string; obj: TObject);
  end;

implementation

{ FormPrefixAttribute }
constructor FormPrefixAttribute.Create(const aPrefix: String);
begin
  FPrefix := aPrefix;
end;

{ FormSectionAttribute }
constructor FormSectionAttribute.Create(const aSection: String);
begin
  FSection := aSection;
end;

{ TRttiTranslation }
class function TRttiTranslation.GetPrefixAttribute(Obj: TRttiObject): FormPrefixAttribute;
var
  Attr: TCustomAttribute;
begin
  for Attr in Obj.GetAttributes do begin
    if Attr is FormPrefixAttribute then
      exit(FormPrefixAttribute(Attr));
  end;
  result := nil;
end;

class function TRttiTranslation.GetSectionAttribute(Obj: TRttiObject): FormSectionAttribute;
var
  Attr: TCustomAttribute;
begin
  for Attr in Obj.GetAttributes do begin
    if Attr is FormSectionAttribute then
      exit(FormSectionAttribute(Attr));
  end;
  result := nil;
end;

class procedure TRttiTranslation.Load(filename: string; obj: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(filename);
    TRttiTranslation.Load(sl, obj);
  finally
    sl.Free;
  end;
end;

class procedure TRttiTranslation.Load(var sl: TStringList; obj: TObject);
var
  ctx: TRttiContext;
  objType: TRttiType;
  Field: TRttiField;
  FormPrefix: FormPrefixAttribute;
  FormSection: FormSectionAttribute;
  CurrentPrefix, CurrentSection, FieldName, value: string;
  aCheckBox: TCheckBox;
  aButton: TButton;
  aLabel: TLabel;
  aTabSheet: TTabSheet;
  aGroupBox: TGroupBox;
  aSpeedButton: TSpeedButton;
  aMenuItem: TMenuItem;
  aComboBox: TComboBox;
  aListView: TListView;
  i: Integer;
begin
  FormPrefix := nil;
  ctx := TRttiContext.Create;
  try
    objType := ctx.GetType(Obj.ClassInfo);
    for Field in objType.GetFields do begin
      // START BY FINDING FORM PREFIX, SKIP FIELDS UNTIL FOUND
      if not Assigned(FormPrefix) then begin
        FormPrefix := GetPrefixAttribute(Field);
        if Assigned(FormPrefix) then
          CurrentPrefix := FormPrefix.Prefix
        else
          continue;
      end;

      // IF FORM SECTION, DUMP SECTION
      FormSection := GetSectionAttribute(Field);
      if Assigned(FormSection) then
        CurrentSection := FormSection.Section;

      // SKIP ALL ITEMS IN 'DontTranslate' SECTION
      if CurrentSection = 'DontTranslate' then
        continue;

      // LOAD VALUES
      FieldName := Field.FieldType.Name;
      if FieldName = 'TCheckBox' then begin
        aCheckBox := TCheckBox(field.GetValue(obj).AsType<TCheckBox>);
        if Assigned(aCheckBox) then begin
          value := ReadValue(CurrentPrefix, sl, Field, 'Caption');
          if value <> '' then aCheckBox.Caption := value;
          value := ReadValue(CurrentPrefix, sl, Field, 'Hint');
          aCheckBox.ShowHint := value <> '';
          if aCheckBox.ShowHint then aCheckBox.Hint := value;
        end;
      end
      else if FieldName = 'TButton' then begin
        aButton := TButton(field.GetValue(obj).AsType<TButton>);
        if Assigned(aButton) then begin
          value := ReadValue(CurrentPrefix, sl, Field, 'Caption');
          if value <> '' then aButton.Caption := value;
          value := ReadValue(CurrentPrefix, sl, Field, 'Hint');
          aButton.ShowHint := value <> '';
          if aButton.ShowHint then aButton.Hint := value;
        end;
      end
      else if FieldName = 'TLabel' then begin
        aLabel := TLabel(field.GetValue(obj).AsType<TLabel>);
        if Assigned(aLabel) then begin
          value := ReadValue(CurrentPrefix, sl, Field, 'Caption');
          if value <> '' then aLabel.Caption := value;
          value := ReadValue(CurrentPrefix, sl, Field, 'Hint');
          aLabel.ShowHint := value <> '';
          if aLabel.ShowHint then aLabel.Hint := value;
        end;
      end
      else if FieldName = 'TTabSheet' then begin
        aTabSheet := TTabSheet(field.GetValue(obj).AsType<TTabSheet>);
        if Assigned(aTabSheet) then begin
          value := ReadValue(CurrentPrefix, sl, Field, 'Caption');
          if value <> '' then aTabSheet.Caption := value;
          value := ReadValue(CurrentPrefix, sl, Field, 'Hint');
          aTabSheet.ShowHint := value <> '';
          if aTabSheet.ShowHint then aTabSheet.Hint := value;
        end;
      end
      else if FieldName = 'TGroupBox' then begin
        aGroupBox := TGroupBox(field.GetValue(obj).AsType<TGroupBox>);
        if Assigned(aGroupBox) then begin
          value := ReadValue(CurrentPrefix, sl, Field, 'Caption');
          if value <> '' then aGroupBox.Caption := value;
          value := ReadValue(CurrentPrefix, sl, Field, 'Hint');
          aGroupBox.ShowHint := value <> '';
          if aGroupBox.ShowHint then aGroupBox.Hint := value;
        end;
      end
      else if FieldName = 'TSpeedButton' then begin
        aSpeedButton := TSpeedButton(field.GetValue(obj).AsType<TSpeedButton>);
        if Assigned(aSpeedButton) then begin
          value := ReadValue(CurrentPrefix, sl, Field, 'Hint');
          aSpeedButton.ShowHint := value <> '';
          if aSpeedButton.ShowHint then aSpeedButton.Hint := value;
        end;
      end
      else if FieldName = 'TMenuItem' then begin
        aMenuItem := TMenuItem(field.GetValue(obj).AsType<TMenuItem>);
        if Assigned(aMenuItem) then begin
          value := ReadValue(CurrentPrefix, sl, Field, 'Caption');
          if value <> '' then aMenuItem.Caption := value;
        end;
      end
      else if FieldName = 'TComboBox' then begin
        aComboBox := TComboBox(field.GetValue(obj).AsType<TComboBox>);
        if Assigned(aComboBox) then begin
          for i := 0 to Pred(aComboBox.Items.Count) do begin
            value := ReadValue(CurrentPrefix, sl, Field, 'Item'+IntToStr(i));
            if value <> '' then aComboBox.Items[i] := value;
          end;
        end;
      end
      else if FieldName = 'TListView' then begin
        aListView := TListView(field.GetValue(obj).AsType<TListView>);
        if Assigned(aListView) then begin
          if not aListView.ShowColumnHeaders then
            continue;
          for i := 0 to Pred(aListView.Columns.Count) do begin
            value := ReadValue(CurrentPrefix, sl, Field, 'Column'+IntToStr(i));
            if value <> '' then aListView.Columns[i].Caption := value;
          end;
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

class function TRttiTranslation.ReadValue(section: string; var sl: TStringList;
  field: TRttiField; subfield: string): string;
var
  name: string;
begin
  // load value from stringlist
  name := Format('%s_%s_%s', [section, field.Name, subfield]);
  Result := StringReplace(sl.Values[name], '#13#10', #13#10, [rfReplaceAll]);
end;

class procedure TRttiTranslation.WriteValue(section: string; value: string; var sl: TStringList;
  field: TRttiField; subfield: string);
var
  name: string;
begin
  if value = '' then
    exit;
  name := Format('%s_%s_%s', [section, field.Name, subfield]);
  sl.Values[name] := StringReplace(value, #13#10, '#13#10', [rfReplaceAll]);
end;

class procedure TRttiTranslation.Save(filename: string; obj: TObject);
var
  ctx: TRttiContext;
  objType: TRttiType;
  Field: TRttiField;
  FormPrefix: FormPrefixAttribute;
  FormSection: FormSectionAttribute;
  sl: TStringList;
  i: Integer;
  Header, CurrentPrefix, CurrentSection, FieldName: string;
  bNewObject: boolean;
  aCheckBox: TCheckBox;
  aButton: TButton;
  aLabel: TLabel;
  aTabSheet: TTabSheet;
  aGroupBox: TGroupBox;
  aSpeedButton: TSpeedButton;
  aMenuItem: TMenuItem;
  aComboBox: TComboBox;
  aListView: TListView;
begin
  FormPrefix := nil;
  ctx := TRttiContext.Create;
  try
    // LOAD FILE IF IT EXISTS
    sl := TStringList.Create;
    if FileExists(filename) then
      sl.LoadFromFile(filename);

    // ADD HEADER IF NEW OBJECT
    Header := Format('{ %s }', [obj.ClassName]);
    bNewObject := sl.IndexOf(Header) = -1;
    if bNewObject then sl.Add(Header);

    try
      objType := ctx.GetType(Obj.ClassInfo);
      for Field in objType.GetFields do begin
        // START BY FINDING FORM PREFIX, SKIP FIELDS UNTIL FOUND
        if not Assigned(FormPrefix) then begin
          FormPrefix := GetPrefixAttribute(Field);
          if Assigned(FormPrefix) then
            CurrentPrefix := FormPrefix.Prefix
          else
            continue;
        end;

        // IF FORM SECTION, DUMP SECTION
        FormSection := GetSectionAttribute(Field);
        if Assigned(FormSection) then begin
          CurrentSection := FormSection.Section;
          if CurrentSection = 'DontTranslate' then
            continue;
          Header := Format('{ ## %s ## }', [FormSection.Section]);
          if (sl.IndexOf(Header) = -1) then sl.Add(Header);
        end;

        // SKIP ALL ITEMS IN 'DontTranslate' SECTION
        if CurrentSection = 'DontTranslate' then
          continue;

        // HANDLE COMPONENTS
        FieldName := Field.FieldType.Name;
        // Handle TCheckBox
        if FieldName = 'TCheckBox' then begin
          aCheckBox := TCheckBox(field.GetValue(obj).AsType<TCheckBox>);
          if Assigned(aCheckBox) then begin
            WriteValue(CurrentPrefix, aCheckBox.Caption, sl, field, 'Caption');
            WriteValue(CurrentPrefix, aCheckBox.Hint, sl, field, 'Hint');
          end;
        end
        // Handle TButton
        else if FieldName = 'TButton' then begin
          aButton := TButton(field.GetValue(obj).AsType<TButton>);
          if Assigned(aButton) then begin
            WriteValue(CurrentPrefix, aButton.Caption, sl, field, 'Caption');
            WriteValue(CurrentPrefix, aButton.Hint, sl, field, 'Hint');
          end;
        end
        // Handle TLabel
        else if FieldName = 'TLabel' then begin
          aLabel := TLabel(field.GetValue(obj).AsType<TLabel>);
          if Assigned(aLabel) then begin
            WriteValue(CurrentPrefix, aLabel.Caption, sl, field, 'Caption');
            WriteValue(CurrentPrefix, aLabel.Hint, sl, field, 'Hint');
          end;
        end
        // Handle TTabSheet
        else if FieldName = 'TTabSheet' then begin
          aTabSheet := TTabSheet(field.GetValue(obj).AsType<TTabSheet>);
          if Assigned(aTabSheet) then begin
            WriteValue(CurrentPrefix, aTabSheet.Caption, sl, field, 'Caption');
            WriteValue(CurrentPrefix, aTabSheet.Hint, sl, field, 'Hint');
          end;
        end
        // Handle TGroupBox
        else if FieldName = 'TGroupBox' then begin
          aGroupBox := TGroupBox(field.GetValue(obj).AsType<TGroupBox>);
          if Assigned(aGroupBox) then begin
            WriteValue(CurrentPrefix, aGroupBox.Caption, sl, field, 'Caption');
            WriteValue(CurrentPrefix, aGroupBox.Hint, sl, field, 'Hint');
          end;
        end
        // Handle TSpeedButton
        else if FieldName = 'TSpeedButton' then begin
          aSpeedButton := TSpeedButton(field.GetValue(obj).AsType<TSpeedButton>);
          if Assigned(aSpeedButton) then begin
            WriteValue(CurrentPrefix, aSpeedButton.Caption, sl, field, 'Caption');
            WriteValue(CurrentPrefix, aSpeedButton.Hint, sl, field, 'Hint');
          end;
        end
        // Handle TMenuItem
        else if FieldName = 'TMenuItem' then begin
          aMenuItem := TMenuItem(field.GetValue(obj).AsType<TMenuItem>);
          if Assigned(aMenuItem) then
            WriteValue(CurrentPrefix, aMenuItem.Caption, sl, field, 'Caption');
        end
        // Handle TComboBox
        else if FieldName = 'TComboBox' then begin
          aComboBox := TComboBox(field.GetValue(obj).AsType<TComboBox>);
          if Assigned(aComboBox) then
            for i := 0 to Pred(aComboBox.Items.Count) do
              WriteValue(CurrentPrefix, aComboBox.Items[i], sl, field, 'Item'+IntToStr(i));
        end
        // Handle TListView
        else if FieldName = 'TListView' then begin
          aListView := TListView(field.GetValue(obj).AsType<TListView>);
          if Assigned(aListView) then begin
            if not aListView.ShowColumnHeaders then
              continue;
            for i := 0 to Pred(aListView.Columns.Count) do
              WriteValue(CurrentPrefix, aListView.Columns[i].Caption, sl, field, 'Column'+IntToStr(i));
          end;
        end;
      end;
    finally
      if bNewObject then sl.Add(' ');
      ForceDirectories(ExtractFilePath(filename));
      sl.SaveToFile(fileName);
      sl.Free;
    end;
  finally
    ctx.Free;
  end;
end;

end.
