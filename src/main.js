const { updateElectronApp } = require('update-electron-app');
updateElectronApp();


const { app, BrowserWindow, ipcMain, dialog } = require('electron/main');
const path = require('node:path');
const homedir = require('os').homedir();
const fs = require ('node:fs');


const preloadPath = path.join(__dirname, 'preload.js');

const metaSavePath = path.join(homedir, 'meta-save.json');

let savePath = '';

function setSavePath(path) {
  if (path != savePath && path != '') {
    savePath = path;
    fs.writeFile(metaSavePath, savePath, err => {
      if (err) console.error(err);
    });
  }
}

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
    autoHideMenuBar: true,
    webPreferences: {
      preload: preloadPath
    }
  });

  function save(pickFile, obj) {
    let temp = savePath;
    if (pickFile || savePath === '') {
      temp = dialog.showSaveDialogSync();
    }
    if (temp === '') {
      console.log("cancelling save operation");
    } else {
      fs.writeFile(temp, JSON.stringify(obj), err => {
        if (err) {
          console.error(err);
        } else {
          setSavePath(temp);
        }
      });
    }
  }

  function load(pickFile) {
    let temp = savePath;
    if (pickFile || savePath === '') {
      temp =
        dialog.showOpenDialogSync({
          properties: ['openFile'],
        }) ?. at(0) || '';
    }
    if (temp === '') {
      console.log("cancelling load operation");
    } else {
      fs.readFile(temp, 'utf8', (err, data) => {
        if (err) {
          console.error(err);
        } else {
          console.log(data);
          controlsWin.webContents.send('controls-receive', JSON.parse(data));
          setSavePath(temp);
        }
      });
    }
  }

  ipcMain.on('controls-send', (_event, obj) => {
    switch (obj.cmdString) {
      case "send":
        displayWin.webContents.send('display-receive', obj);
        break;
      case "save":
        save(false, obj);
        break;
      case "save as":
        save(true, obj);
        break
      case "load":
        load(true);
        break;
      case "reload":
        load(false);
        break;
      case "show":
        displayWin.show();
        break;
      case "hide":
        displayWin.hide();
        break;
      case "save and quit":
        save(false, obj);
        app.quit();
        break;
    }
  });

  controlsWin.loadFile('src/controls.html');
  displayWin.loadFile('src/display.html');

  fs.readFile(metaSavePath, 'utf8', (err, data) => {
    if (err) {
      console.error(err);
    } else {
      savePath = data;
    }
  });

}


app.whenReady().then(() => {
  createWindows();
});


