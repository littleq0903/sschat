apikey=`./api_register_post.sh`
roomid=`./api_room_post.sh $apikey 2> /dev/null`
user1_id=`./api_user_post.sh $apikey 2> /dev/null`
user2_id=`./api_user_post.sh $apikey 2> /dev/null`

# put
./api_room_put.sh $apikey $roomid $user1_id &> /dev/null
./api_room_put.sh $apikey $roomid $user2_id &> /dev/null

echo Room ID
echo $roomid
echo User 1
echo $user1_id
echo User 2
echo $user2_id 
