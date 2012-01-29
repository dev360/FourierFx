import zmq
from zmq.utils.strtypes import asbytes

class ZmqSocket(object):
    """ Abstraction for a zero mq socket """

    def __init__(self, *args, **kwargs):

        socket_types = {
            'XPUB': zmq.XPUB,
            'XSUB': zmq.XSUB
        }

        bind = kwargs.get('bind', True)
        connect = kwargs.get('connect', True)

        str_socket_type = args[1].upper()

        self.host = args[0]
        self.socket_type = socket_types[str_socket_type]

        self.context = zmq.Context()
        self.socket = self.context.socket(self.socket_type)

        if self.socket_type == zmq.XPUB and bind:
            self.bind()

        if self.socket_type == zmq.XSUB and connect:
            self.connect()

    def bind(self):
        """ Binds the socket """
        self.socket.bind(self.host)
        print 'Binding to {0}'.format(self.host)

    def connect(self):

        self.socket.connect(self.host)
        print 'Connected to {0}'.format(self.host)
        #if self.socket_type == zmq.XSUB:
            #self.socket.setsockopt(zmq.SUBSCRIBE, u''.encode('utf-8'))
                    

    def receive(self):
        print 'receiving'
        return self.socket.recv_pyobj()


    def send(self, message):
        """ Sends a message to the socket """
        self.socket.send_unicode(message)

