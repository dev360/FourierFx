#coding=utf-8

from django.contrib.auth.views import password_change, password_reset
from django.conf.urls.defaults import *
from django.utils.http import urlquote

from core import views

urlpatterns = patterns('',

    url(r'^$', views.index, name='index'),    
)

