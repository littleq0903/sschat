import websocket

WS_SERVER_HOST = 'localhost:8001'
WS_API_LOCATION = '/websocket/polling'

def on_message(ws, message):
    print "[WebSocket:message] received: %s" % message

def on_error(ws, error):
    print "[WebSocket:error] %s" % error

def on_close(ws):
    print '### closed ###'

def on_open(ws):
    print '### open ###'

if __name__ == '__main__':
    websocket.enableTrace(True)
    ws = websocket.WebSocketApp('ws://%s%s' % (WS_SERVER_HOST, WS_API_LOCATION),
            on_message = on_message,
            on_error = on_error,
            on_close = on_close)
    ws.on_open = on_open
    ws.run_forever()
    while 1:
        send_msg = raw_input("send")
        ws.send(send_msg)


