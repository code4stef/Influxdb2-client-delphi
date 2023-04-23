Driver Delphi for influxDB 2.0

Based on GoLang driver

Motivation of this project :

There is no official driver for InfluxDB 2.X in Delphi ( since this one )

Work in progress :

[ ] Setup
[ ] SetupWithToken
[X] Ready()
[X] Health()
[X] Ping()
[ ] Close()
[ ] Options()
[X] ServerURL()
[ ] HTTPService() http.Service
[ ] WriteAPI(org, bucket : string)
[X] WriteAPIBlocking(org, bucket : string) : IWriteAPI;
[ ] QueryAPI(org string) api.QueryAPI
[ ] AuthorizationsAPI() api.AuthorizationsAPI
[ ] OrganizationsAPI() api.OrganizationsAPI
[ ] UsersAPI() api.UsersAPI
[ ] DeleteAPI() api.DeleteAPI
[ ] BucketsAPI() api.BucketsAPI
[ ] LabelsAPI() api.LabelsAPI
[ ] TasksAPI() api.TasksAPI
[ ] APIClient() *domain.Client




