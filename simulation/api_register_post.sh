. ./server_env.sh

curl -X POST "$SERVER_HOST/api/register" | sed -r "s/.*api_key\":\"(\w+)\".*/\1/g" 2> /dev/null
