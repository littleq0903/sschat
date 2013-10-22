. ./server_env.sh

curl --data "" "$SERVER_HOST/api/register" 2> /dev/null | sed -r "s/.*api_key\":\"(\w+)\".*/\1/g"
