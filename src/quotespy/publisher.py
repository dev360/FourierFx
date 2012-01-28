from utils.zeromq import ZmqSocket

class QuotePublisher(object):

    def __init__(self, *args, **kwargs):
        self.socket = ZmqSocket('tcp://127.0.0.1:6000', 'XPUB')
        super(QuotePublisher, self).__init__(*args, **kwargs)

    def send(self, quote):
        self.socket.send(quote)
