. ./server_env.sh

if [ -n "$1" ]; then
    API_KEY="$1"
    RESPONSE=`curl -X POST "$SERVER_HOST/api/user?apikey=$API_KEY"`
    PARSED_RESPONSE=`echo "$RESPONSE" | sed -r "s/.*user_id\":\"(\w+)\".*/\1/g"`
    echo "$PARSED_RESPONSE"
else
    echo "Please input your api key"
fi


