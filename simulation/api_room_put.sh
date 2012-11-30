. ./server_env.sh


if [ -n "$1" ] && [ -n "$2" ] && [ -n "$3" ]; then
    API_KEY="$1"
    room="$2"
    user="$3"
    RESPONSE=`curl -X PUT "$SERVER_HOST/api/room/$room/$user/add?apikey=$API_KEY"`
    RESPONSE=`echo "$RESPONSE" | sed -r "s/.*room_id\":\"(\w+)\".*/\1/g"`
    echo "$RESPONSE"
else
    echo "Please input your api key"
fi


