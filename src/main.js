const { app, BrowserWindow, ipcMain } = require('electron/main')
const path = require('node:path')
const fs = require ('node:fs')


const preloadPath = path.join(__dirname, 'preload.js');

const savefilePath = path.join(__dirname, '../save-data.json');


async function createWindows() {
  const controlsWin = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: preloadPath
    }
  });

  const displayWin = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: preloadPath
    }
  });

  ipcMain.on('controls-send', (_event, obj) => {
    switch (obj.cmdString) {
      case "send":
        displayWin.webContents.send('display-receive', obj);
        break;
      case "save":
        fs.writeFile(savefilePath, JSON.stringify(obj), err => {
          if (err) console.error(err);
        });
        break;
      case "load":
        fs.readFile(savefilePath, 'utf8', (err, data) => {
          if (err) console.error(err);
          controlsWin.webContents.send('controls-receive', JSON.parse(data));
        });
        break;
      case "show":
        displayWin.show();
        break;
      case "hide":
        displayWin.hide();
        break;
    }
  });

  controlsWin.loadFile('src/controls.html');
  displayWin.loadFile('src/display.html');
}


app.whenReady().then(() => {
  createWindows();
});


