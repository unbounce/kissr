curl -H "Content-Type: application/json" -X POST -d '{    "sort":"0",    "order":"asc",    "product_id":"YOUR_PRODUCT_ID",    "query_params": {      "type":"group",      "filter":{"type":"and", "operands":[{"type":"event","negate":false,"event":72,"frequencyValue":1,"frequencyOccurance":"at_least","comparisonMode":"any_value"}], "options": {"defaultDateRange":{"start_date":"2015-06-01","end_date":"2015-06-02"}}},      "defaultCalculationDateRange":{"dateRangeId":"custom","startDate":"2015-05-31T17:00:00-0700","endDate":"2015-06-01T17:00:00-0700"},      "calculations":[{"type":"first_date_in_range","subject":{"type":"event","negate":false,"event":6,"frequencyValue":1,"frequencyOccurance":"at_least","version":2,"options":{}},"label":"First time of visited site"}]    }  }' -H "Authorization: Bearer YOUR_API_TOKEN" -i "https://query.kissmetrics.com/v2/products/YOUR_PRODUCT_ID/reports/people_search"

// Note the url needs to be extracted from the link returned above
curl -H "Authorization: Bearer YOUR_API_TOKEN" -i https://query.kissmetrics.com/v2/queries/099cc7cc-7fda-4284-8412-b64bd7b01bdb/status

// Note the url needs to be extracted from the results link returned above
curl -H "Authorization: Bearer YOUR_API_TOKEN" -i https://query.kissmetrics.com/v2/queries/099cc7cc-7fda-4284-8412-b64bd7b01bdb/results
