unit RttiJson;

interface

uses
  SysUtils, Rtti,
  // superobject json library
  superobject;
  
type
  TRttiJson = class (TObject)
  public
    class function ToJson(obj: TObject): string;
    class function FromJson(json: string; classType: TClass): TObject;
  end;

implementation

class function TRttiJson.ToJson(obj: TObject): string;
var
  rtype: TRTTIType;
  field: TRTTIField;
  fieldType: string;
  jsonObj: ISuperObject;
  date: TDateTime;
begin
  jsonObj := SO;
  rtype := TRTTIContext.Create.GetType(obj.ClassType);

  // loop through fields
  for field in rType.GetFields do begin
    fieldType := field.FieldType.ToString;
    // handle datatypes I use
    if (fieldType = 'string') then
      jsonObj.S[field.Name] := field.GetValue(obj).ToString
    else if (fieldType = 'Integer') then
      jsonObj.I[field.Name] := field.GetValue(obj).AsInteger
    else if (fieldType = 'TDateTime') then begin
      date := StrToFloat(field.GetValue(obj).ToString);
      jsonObj.S[field.Name] := DateTimeToStr(date);
    end;
  end;

  Result := jsonObj.AsJSon;
end;

{
  Example usage:
  report := TReport(FromJson(reportJson, TReport));
}
class function TRttiJson.FromJson(json: string; classType: TClass): TObject;
var
  rtype: TRTTIType;
  field: TRTTIField;
  fieldType: string;
  context: TRTTIContext;
  jsonObj: ISuperObject;
  date: TDateTime;
begin
  jsonObj := SO(PChar(json));
  context := TRTTIContext.Create;
  rtype := context.GetType(classType);
  Result := classType.Create;

  // loop through fields
  for field in rType.GetFields do begin
    fieldType := field.FieldType.ToString;
    // handle datatypes I use
    if (fieldType = 'string') then
      field.SetValue(Result, jsonObj.S[field.Name])
    else if (fieldType = 'Integer') then
      field.SetValue(Result, jsonObj.I[field.Name])
    else if (fieldType = 'TDateTime') then begin
      date := StrToDateTime(jsonObj.S[field.Name]);
      field.SetValue(Result, TValue.From<TDateTime>(date));
    end;
  end;

  context.Free;
end;

end.