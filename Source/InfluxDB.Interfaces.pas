unit InfluxDB.Interfaces;


interface
uses InfluxDB.Types;

type

  IInfluxClient = interface
    ['{350E4CAA-913A-4266-9C56-D45169117703}']
    // Setup sends request to initialise new InfluxDB server with user, org and bucket, and data retention period
    // and returns details about newly created entities along with the authorization object.
    // Retention period of zero will result to infinite retention.
    //function Setup( username, password, org, bucket : string;  retentionPeriodHours : integer) : domain.OnboardingResponse;
    // SetupWithToken sends request to initialise new InfluxDB server with user, org and bucket, data retention period and token
    // and returns details about newly created entities along with the authorization object.
    // Retention period of zero will result to infinite retention.
    //function SetupWithToken( username, password, org, bucket : string; retentionPeriodHours : integer; token : string) : domain.OnboardingResponse;
    // Ready returns InfluxDB uptime info of server. It doesn't validate authentication params.
    function Ready() : TReady;
    // Health returns an InfluxDB server health check result. Read the HealthCheck.Status field to get server status.
    // Health doesn't validate authentication params.
    function Health() : THealthCheck;
    // Ping validates whether InfluxDB server is running. It doesn't validate authentication params.
    function Ping(): boolean;
    // Close ensures all ongoing asynchronous write clients finish.
    // Also closes all idle connections, in case of HTTP client was created internally.
    procedure Close();
    // Options returns the options associated with client
    //Options() *Options
    // ServerURL returns the url of the server url client talks to
    function ServerURL() : string;
    // HTTPService returns underlying HTTP service object used by client
    //HTTPService() http.Service
    // WriteAPI returns the asynchronous, non-blocking, Write client.
    // Ensures using a single WriteAPI instance for each org/bucket pair.
    //function WriteAPI(org, bucket string) api.WriteAPI
    // WriteAPIBlocking returns the synchronous, blocking, Write client.
    // Ensures using a single WriteAPIBlocking instance for each org/bucket pair.
    //WriteAPIBlocking(org, bucket string) api.WriteAPIBlocking
    // QueryAPI returns Query client.
    // Ensures using a single QueryAPI instance each org.
    //function QueryAPI(org string) api.QueryAPI
    // AuthorizationsAPI returns Authorizations API client.
    //AuthorizationsAPI() api.AuthorizationsAPI
    // OrganizationsAPI returns Organizations API client
    //OrganizationsAPI() api.OrganizationsAPI
    // UsersAPI returns Users API client.
    //UsersAPI() api.UsersAPI
    // DeleteAPI returns Delete API client
    //DeleteAPI() api.DeleteAPI
    // BucketsAPI returns Buckets API client
    //function BucketsAPI() api.BucketsAPI
    // LabelsAPI returns Labels API client
    //LabelsAPI() api.LabelsAPI
    // TasksAPI returns Tasks API client
    //TasksAPI() api.TasksAPI

   // APIClient() *domain.Client
  end;





implementation

end.
