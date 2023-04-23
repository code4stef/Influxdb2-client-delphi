unit InfluxDB.Types;

interface

uses REST.Json.Types,
     System.RTTI,
     System.Generics.Collections;


// InfluxDB Error structure

type TError = record
	Err        : boolean;
	StatusCode : integer;
	Code       : string;
	Message    : string;
	RetryAfter : cardinal;
end;

function NewError( error : boolean = false) : TError;

// ----------------- Health -------------

// URL/health

type HealthCheckStatus = string;
// HealthCheckStatus defines model for HealthCheck.Status.

const
	HealthCheckStatusFail = 'fail';
	HealthCheckStatusPass = 'pass';

type THealthCheck = class
  public
  [JSONName('checks')]
	Checks  :  Array of THealthCheck;   // `json:"checks,omitempty"`
  [JSONName('commit')]
	Commit  : string; //           `json:"commit,omitempty"`
  [JSONName('message')]
	Message : string; //           `json:"message,omitempty"`
  [JSONName('name')]
	Name    : string; //            `json:"name"`
  [JSONName('status')]
	Status  : HealthCheckStatus; // `json:"status"`
  [JSONName('version')]
	Version : string; //           `json:"version,omitempty"`
end;

// --------------- READY ----------

type ReadyStatus = string;

const ReadyStatusReady = 'ready';

type TReady = class
  [JSONName('started')]
	Started : TDateTime; //   `json:"started,omitempty"`
  [JSONName('status')]
	Status  : ReadyStatus; // `json:"status,omitempty"`
  [JSONName('up')]
	Up      : string; //     `json:"up,omitempty"`
end;





// Bucket defines model for Bucket.
TBucket = class
public
  [JSONName('createdAt')]
  CreatedAt  : TDateTime;// Tj*time.Time `json:"createdAt,omitempty"`
  [JSONName('description')]
  Description : string; //    `json:"description,omitempty"`
  [JSONName('id')]
  Id :          string; //    `json:"id,omitempty"`
  (*Labels      *Labels    `json:"labels,omitempty"`
	Links       *struct {
		// URI of resource.
		Labels *Link `json:"labels,omitempty"`

		// URI of resource.
		Members *Link `json:"members,omitempty"`

		// URI of resource.
		Org *Link `json:"org,omitempty"`

		// URI of resource.
		Owners *Link `json:"owners,omitempty"`

		// URI of resource.
		Self *Link `json:"self,omitempty"`

		// URI of resource.
		Write *Link `json:"write,omitempty"`
	} `json:"links,omitempty"`    *)
  [JSONName('name')]
	Name  : string; // `json:"name"`
  [JSONName('ordID')]
	OrgID  : string; // `json:"orgID,omitempty"`

	// Retention rules to expire or retain data.
	// #### InfluxDB Cloud
	//
	// - `retentionRules` is required.
	//
	// #### InfluxDB OSS
	//
	// - `retentionRules` isn't required.
	//RetentionRules RetentionRules `json:"retentionRules"`
  [jsonname('rp')]
	Rp             : string ;//       `json:"rp,omitempty"`
 //	SchemaType     *SchemaType    `json:"schemaType,omitempty"`
//  [jsonName('type')]
//	BucketType   :  TBucketType ;//   `json:"type,omitempty"`
  [jsonname('updatedAt')]
	UpdatedAt    :  TDateTime; //     `json:"updatedAt,omitempty"`
end;


// Class to manage an Influx DB point

type TPointPrecision = (pDefaultNanoSeconds, pMicrosecond, pMillisecond, pSecond );

     function PointPrecisionToString (precision : TPointPrecision ) :string;

type

// Options holds write configuration properties

TWriteOptions = class
public
	// Maximum number of points sent to server in single request. Default 5000
	batchSize : cardinal;
	// Interval, in ms, in which is buffer flushed if it has not been already written (by reaching batch size) . Default 1000ms
	flushInterval : cardinal;
	// Precision to use in writes for timestamp. In unit of duration: time.Nanosecond, time.Microsecond, time.Millisecond, time.Second
	// Default time.Nanosecond
	precision : TPointPrecision;
	// Whether to use GZip compression in requests. Default false
	useGZip : boolean;
	// Tags added to each point during writing. If a point already has a tag with the same key, it is left unchanged.
	defaultTags : TDictionary<string,string>;
	// Default retry interval in ms, if not sent by server. Default 5,000.
	retryInterval : cardinal;
	// Maximum count of retry attempts of failed writes, default 5.
	maxRetries : cardinal;
	// Maximum number of points to keep for retry. Should be multiple of BatchSize. Default 50,000.
	retryBufferLimit : cardinal;
	// The maximum delay between each retry attempt in milliseconds, default 125,000.
	maxRetryInterval : cardinal;
	// The maximum total retry timeout in millisecond, default 180,000.
	maxRetryTime : cardinal;
	// The base for the exponential retry delay
	exponentialBase : cardinal;
	// InfluxDB Enterprise write consistency as explained in https://docs.influxdata.com/enterprise_influxdb/v1.9/concepts/clustering/#write-consistency
	consistency : string;

  constructor Create;
  destructor Destroy;
end;

type
  IInfluxPoint = interface
    ['{8BD08842-D0C4-476E-A5A6-502EEA494026}']
    function AddField(AField: String; AValue: TValue): IInfluxPoint;
    function AddTag(ATag, AValue: String): IInfluxPoint;
    function SetTime(ATimeStamp: TDateTime): IInfluxPoint;
    function PointToLineProtocol ( Precision : TPointPrecision = pDefaultNanoSeconds): String;
  end;

  TInfluxPoint = class(TInterfacedObject,IInfluxPoint )
    Measurement: String;
    Tags: TArray<TPair<String, String>>;
    Fields: TArray<TPair<String, TValue>>;
    TimeStamp: TDateTime;
    constructor Create(AMeasurement: String; ATimeStamp: TDateTime = 0);
    function AddField(AField: String; AValue: TValue): IInfluxPoint;
    function AddTag(ATag, AValue: String): IInfluxPoint;
    function SetTime(ATimeStamp: TDateTime): IInfluxPoint;
    function PointToLineProtocol ( Precision : TPointPrecision = pDefaultNanoSeconds): String;
  end;

  function NewPointWithMeasurement(measurement : string) : TInfluxPoint;
  function NewPoint( measurement : string; tags : Array of TPair<String, String>; fields : Array of TPair<String, TValue>; ts : TDateTime ) : IInfluxPoint;

// BucketType defines model for Bucket.Type.
//type BucketType string


implementation
uses   System.SysUtils,
       System.StrUtils,
       System.DateUtils;

function NewError(Error : boolean = false) : TError;
begin
  Result.StatusCode := 0;
  Result.Code       := '';
  Result.Message    := '';
  Result.Err        := Error;
	Result.RetryAfter := 0;
end;

function PointPrecisionToString (precision : TPointPrecision ) :string;
begin
  result := '';
  case precision of
    pMicrosecond : result := 'us';
    pMillisecond : result := 'ms';
    pSecond      : result := 's';
  else
    Result := 'ns';
  end;
end;

{ TInfluxDBValue }

function TInfluxPoint.AddField(AField: String; AValue: TValue): IInfluxPoint;
var
  Val: TPair<String, TValue>;
  Res: integer;
  i : integer;
begin
  Val.Key := AField;
  Val.Value := AValue;

  // Look if Field already exists
  for i:= low(Fields) to high(Fields) do
  begin
    if (Fields[i].Key=AField) then
    begin
      Fields[i].Value := AValue;
      Result := self;
      Exit;
    end;
  end;

  Res := Length(Fields);
  SetLength(Fields, Res +1);
  Fields[Res] := Val;

  Result := self;
end;

function TInfluxPoint.AddTag(ATag, AValue: String): IInfluxPoint;
var
  Val: TPair<String, String>;
  Res : integer;
  i : integer;
begin
  Val.Key := ATag;
  Val.Value := AValue;

  // Look if Tag already exists
  for i:= low(Tags) to high(Tags) do
  begin
    if (Tags[i].Key=ATag) then
    begin
      Tags[i].Value := AValue;
      Result := self;
      Exit;
    end;
  end;

  Res := Length(Tags);
  SetLength(Tags, Res +1);
  Tags[Res] := Val;

  Result := self;
end;

function escapeKey(key : string; escapeEqual : boolean) : string;
var i : integer;
begin
  Result := '';
	for i := 1 to Length(key) do
  begin
    if (key[i] = chr($0A)) then
    begin
      Result := Result + '\\n';
      continue;
    end;
    if (key[i] = chr($0D)) then
    begin
      Result := Result + '\\r';
      continue;
    end;
    if (key[i] = chr($09)) then
    begin
      Result := Result + '\\t';
      continue;
    end;
    if ((key[i] = ' ') or (key[i]=',')) then
    begin
      Result := Result + '\';
    end;
    if (key[i] = '=') then
    begin
      if escapeEqual then
        Result := Result + '\';
    end;
	  Result := Result + key[i];
  end;
end;

function escapeValue(value : string) : string;
var i : integer;
begin
  Result := '';
	for i := 1 to length(value) do
  begin
    if ( (value[i] = '\') or (value[i] = '"') ) then
      Result := Result + '\';
		Result := Result + value[i];
  end;
end;


function TInfluxPoint.PointToLineProtocol( Precision : TPointPrecision = pDefaultNanoSeconds ): String;
var
  ATag: TPair<String, String>;
  AField: TPair<String, TValue>;
  Val, S: String;
  memDecimalSep : char;
  DT : Int64;
begin
  // Memorise and force '.' to be decimal separator
  memDecimalSep := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';

  Result := escapeKey(Measurement, false) ;
  for ATag in Tags do
    Result := Result + ',' + escapeKey(ATag.Key, True) + '=' + escapeKey(ATag.Value, True);

  Result := Result + ' ';

  S := '';
  for AField in Fields do
  begin
    if AField.Value.Kind in [tkInteger, tkInt64, tkFloat, tkEnumeration] then
    begin
      Val := escapeKey(AField.Key, True) + '=' + escapeValue(AField.Value.ToString);
      if AField.Value.Kind in [tkInteger, tkInt64] then
        Val := Val + 'i';
    end
    else
      Val := escapeKey(AField.Key, True) + '=' + escapeValue(AField.Value.ToString.QuotedString('"'));
    S := IfThen(S = '', Val, String.Join(',', [S, Val]));
  end;

  Result := Result + S;

  if TimeStamp > 0 then
  begin
    case Precision of
      pMicrosecond : DT := System.DateUtils.DateTimeToUnix(TimeStamp) *1000;
      pMillisecond : DT := System.DateUtils.DateTimeToUnix(TimeStamp) * 1000000;
      pSecond      : DT := System.DateUtils.DateTimeToUnix(TimeStamp);
    else
       DT := System.DateUtils.DateTimeToUnix(TimeStamp) * 1000000000;  // Nano
    end;
    Result := Result + ' ' + DT.ToString;

  end;
  // Set back current decimal separator
  FormatSettings.DecimalSeparator := memDecimalSep;
end;


// SetTime set timestamp for a Point.
function TInfluxPoint.SetTime(ATimeStamp: TDateTime) : IInfluxPoint;
begin
  self.TimeStamp := ATimeStamp;
	Result := self;
end;


constructor TInfluxPoint.Create(AMeasurement: String; ATimeStamp: TDateTime);
begin
  SetLength(Tags, 0);
  SetLength(Fields, 0);
  Measurement := AMeasurement;
  TimeStamp := ATimeStamp;
end;

// NewPointWithMeasurement creates a empty Point
// Use AddTag and AddField to fill point with data
function NewPointWithMeasurement(measurement : string) : TInfluxPoint;
begin
	Result := TInfluxPoint.Create(measurement);
end;



// NewPoint creates a Point from measurement name, tags, fields and a timestamp.
function NewPoint(
    measurement : string;
    tags : Array of TPair<String, String>;
    fields : Array of TPair<String, TValue>;
    ts : TDateTime
  ) : IInfluxPoint;
var tag    :  TPair<String, String>;
    field  :  TPair<String, TValue>;
begin
  Result := TInfluxPoint.Create(measurement, ts);

  // Add tags
  if length(tags)>0 then
  begin
  	for tag in tags do
    begin
      Result.AddTag(tag.Key,tag.Value);
		end;
  end;

  // Add fields
  if length(fields)>0 then
  begin
  	for field in fields do
    begin
      Result.AddField(field.Key,field.Value);
		end;
  end;

 // Result.SortFields();
 //	Result.SortTags();

end;

{ TWriteOptions }

constructor TWriteOptions.Create;
begin
  defaultTags := TDictionary<string,string>.Create;
end;

destructor TWriteOptions.Destroy;
begin
  defaultTags.Free;
end;

end.
