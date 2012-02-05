from threading import Lock

import redis

# Sorry but this is neccessary to get the settings imported correctly
try:
    from .. import settings
except ValueError:
    import settings


def get_redis_pool():

    if not settings.REDIS_POOL:
        lock = Lock()
        with lock:
            if not settings.REDIS_POOL:
                settings.REDIS_POOL = redis.ConnectionPool(host=settings.REDIS_HOST, \
                        port=settings.REDIS_PORT, db=0)

        return settings.REDIS_POOL

class RedisDB(object):
    """
    Utility class to get a Redis connection that is ready to be used
    and that reads the correct connection settings from config.
    """

    def __init__(self):
        self.redis = redis.Redis(connection_pool=get_redis_pool())

    def __getattribute__(self, name):
        if name in ('redis',):
            return super(RedisDB, self).__getattribute__(name)
        else:
            return self.redis.__getattribute__(name)

    def __getattr__(self, name):
        if name in ('redis',):
            return super(RedisDB, self).__getattr__(name)
        return self.redis.__getattr__(name)

    def __setattr__(self, name, value):
        if name in ('redis',):
            return super(RedisDB, self).__setattr__(name,value)
        return self.redis.__setattr__(name, value)

