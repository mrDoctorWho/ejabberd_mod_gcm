mod_gcm
=====

**[Fork Me](https://github.com/mrDoctorWho/ejabberd_mod_gcm/fork) Now! Spread the project for great good!**

mod_gcm is an ejabberd module to send offline messages as PUSH notifications for Android using Google Cloud Messaging API.

> Consider using [mod_push](https://github.com/royneary/mod_push) which implements [XEP-0357](http://xmpp.org/extensions/xep-0357.html) and works with many PUSH services.

This module **has nothing to do** with [XEP-0357](http://xmpp.org/extensions/xep-0357.html).

The main goal of this module is to send all offline messages to the registered (see [Usage](#Usage)) clients via Google Cloud Messaging service.

**Compilation**:

Because of the dependencies such as xml.hrl, logger.hrl, etc it's recommended to compile the module with ejabberd itself: put it in the ejabberd/src directory and run the default compiler.

**Configuration**:

To let the module work fine with Google APIs, put the line below in your ejabberd config file. In case of ejabberd 14, the line is:
```yaml
gcm_api_key: "Your Google APIs key"
```
[Here](https://developer.android.com/google/gcm/gs.html) you can create your own API key for Google Cloud Messaging (you need the server key).
Bear in mind that the feature is highly limited for free users.

**<a name="Usage"></a>Usage (Client to server)**:

As you may know, Google Cloud Messaging **won't work** as you expect without the client part.

You won't find the instructions how to create your own Google Cloud Messaging client here. Although, [this](https://developer.android.com/google/gcm/client.html) example should work fine.

You also need to send this stanza to the server over the XMPP connection, to let the server know your client key:
```xml
<iq to="YourServer" type="set">
  <register xmlns="https://android.googleapis.com/gcm" >
    <key>API_KEY</key>
  </register>
</iq>
```

The key is kept in mnesia database and completely belongs to the JabberID which it was sent from.


**Compatibility**:

The module works fine with Ejabberd 16 and maybe the further versions. For the older ones, checkout the *ejabberd14* branch.
