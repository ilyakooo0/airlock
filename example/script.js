import { fetchEventSource } from "/fetch-event-source.js"

const uid = `${Math.floor(Date.now() / 1000)}-${Math.random()}`;

function searchForInit(obj) {
  if (obj.init) {
    return obj.init;
  } else {
    for (let key in obj) {
      if (obj.hasOwnProperty(key)) {
        const result = searchForInit(obj[key]);
        if (result) {
          return result;
        }
      }
    }
  }
}

let app = searchForInit(Elm)({ node: document.getElementById("elm"), flags: { uid } });

app.ports.createEventSource.subscribe((url) => {
  fetchEventSource(url, {
    headers: {
      Accept: 'application/x-urb-jam',
      "x-channel-format": 'application/x-urb-jam',
      "content-type": 'application/x-urb-jam'
    },
    credentials: 'include',
    responseTimeout: 25000,
    openWhenHidden: true,
    onmessage(ev) {
      console.log(ev)
      app.ports.onEventSourceMessage.send({ message: ev.data });
    },
    onerror(err) {
      console.log(err)
      app.ports.onEventSourceMessage.send({ error: err });
    }
  });

});
