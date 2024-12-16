How to create Grafana Dashboards:
1) Run 'sparql.query' in Blazegraph GUI
2) Paste results in 'sparql.results' (same format as provided example)
3) Run 'bash create_sql_query.sh' in this repository to update placeholders in provided 'sql.query'
4) Run updated SQL query in Adminer GUI
5) Paste results in 'sql.results' (same format as provided example)
6) Log in to Grafana and create new Data Source in Grafana GUI
7) Replace datasource uid in provided .json models with uid of newly created Data Source
8) Run 'bash create_grafana_jsons.sh' in this repository to update placeholders in provided json models
9) Import .json models as new dashboards via Grafana GUI
