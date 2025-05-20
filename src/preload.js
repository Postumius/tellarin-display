const { contextBridge, ipcRenderer } = require('electron/renderer')

contextBridge.exposeInMainWorld('ipc', {
  controlsSender: (model) => ipcRenderer.send('controls-send', model),

  controlsReceiver: (callback) =>
    ipcRenderer.on('controls-receive', (_event, str) => callback(str)),

  displayReceiver: (callback) =>
    ipcRenderer.on('display-receive', (_event, model) => callback(model)),

  //controlsSave: (model) => ipcRenderer.send('controls-save', model),
})
