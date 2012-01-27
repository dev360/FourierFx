import zmq

class ZmqSocket(object):
    """ Abstraction for a zero mq socket """

    def __init__(self, *args, **kwargs):

        socket_types = {
            'XPUB': zmq.XPUB,
            'XSUB': zmq.XSUB
        }

        bind = kwargs.get('bind', True)

        self.bind_to = args[0]
        self.socket_type = socket_types[args[1].upper()]

        self.context = zmq.Context()
        self.socket = self.context.socket(self.socket_type)

        if bind:
            self.bind()

    def bind(self):
        """ Binds the socket """
        self.socket.bind(self.bind_to)

    def send(self, message):
        """ Sends a message to the socket """
        self.socket.send_pyobj(message)

