unit InfluxDB.Types;

interface

uses REST.Json.Types;


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

// BucketType defines model for Bucket.Type.
//type BucketType string


implementation

end.
