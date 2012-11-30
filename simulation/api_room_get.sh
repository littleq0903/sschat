. ./server_env.sh

if [ -n "$1" ] && [ -n "$2" ]; then
    API_KEY="$1"
    room="$2"
    RESPONSE=`curl -X GET "$SERVER_HOST/api/room/$room?apikey=$API_KEY"`
    PARSED_RESPONSE=`echo "$RESPONSE" | sed -r "s/.*room_id\":\"(\w+)\".*/\1/g"`
    echo "$PARSED_RESPONSE"
else
    echo "Please input your api key"
fi


