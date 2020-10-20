async function midiInit(send, subscribe) {
  const midiAccess = await navigator.requestMIDIAccess({ sysex: true })
  const initialInputs = midiAccess.inputs.values()
  const initialOutputs = midiAccess.outputs.values()
  const midiMap = {}

  function sendPort(port) {
    midiMap[port.id] = port
    const message = jsonify(port)
    send(message)
    console.log('sent message to Elm', message)
  }

  function * multiIter(iters) {
    for (const iter of iters) {
      for (const value of iter) {
        yield value
      }
    }
  }
  for (const port of multiIter([initialInputs, initialOutputs])) {
    sendPort(port)
  }

  midiAccess.onstatechange = event => {
    sendPort(event.port)
  }
  subscribe(message => {
    console.log('received message from Elm', JSON.stringify(message))
    const port = midiMap[message.to]
    if (!port) {
      console.error(`invalid port id ${message.to}`)
      return
    }
    if (message.data) {
      if (port.type !== 'output') {
        console.error('port type must be output')
        return
      }
      console.log('sending MIDI message', message)
      port.send(message.data /*, timestamp (delay sending of message) */)
    } else {
      if (port.type !== 'input') {
        console.error('port type must be input')
        return
      }
      if (port.onmidimessage) {
        console.error(`already subscribed to port ${port.id}`)
        return
      }
      port.onmidimessage = event => {
        send({
          // type: 'midiMessage',
          from: port.id,
          data: event.data
        })
        console.log('sent message to Elm', message)
      }
    }
  })

  function jsonify({ connection, id, manufacturer, name, state, type, version }) {
    return { connection, id, manufacturer, name, state, type, version }
  }
}
