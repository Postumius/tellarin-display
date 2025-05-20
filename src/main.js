const { app, BrowserWindow, ipcMain } = require('electron/main')
const path = require('node:path')
const fs = require ('node:fs/promises')

const preloadPath = path.join(__dirname, 'preload.js')

async function save(json) {
  // fs.writeFile('save-data.json', json, err => {
  //   if (err) {
  //     console.error(err);
  //   }
  // });
  try {
    await fs.writeFile('save-data.json', json)
  } catch (err) {
    console.error(err)
  }
}


async function createWindows() {
  const controlsWin = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: preloadPath
    }
  })
  const displayWin = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: preloadPath
    }
  })

  async function load() {
    try {
      const result = JSON.parse(await fs.readFile('save-data.json', {encoding: 'utf8'}));
      console.log(result);
      controlsWin.webContents.send('controls-receive', result);
    } catch (err) {
      console.error(err);
    }
  }

  ipcMain.on('controls-send', (_event, model) => {
    switch (model.cmdString) {
      case "send":
        displayWin.webContents.send('display-receive', model);
        break
      case "save":
        save(JSON.stringify(model));
        break
      case "load":
        load();
        break;
    }
  })

  controlsWin.loadFile('src/controls.html')
  displayWin.loadFile('src/display.html')
}

app.whenReady().then(() => {
  createWindows()
})


