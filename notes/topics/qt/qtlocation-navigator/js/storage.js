//storage.js
// First, let's create a short helper function to get the database connection
.import QtQuick.LocalStorage 2.0 as DB
function getDatabase() {
    return DB.LocalStorage.openDatabaseSync("qtlocation-navigator", "1.0", "StorageDatabase", 100000);
}

// At the start of the application, we can initialize the tables we need if they haven't been created yet
function initializeSettings() {
    var db = getDatabase();
    db.transaction(
        function(tx) {
            // Create the settings table if it doesn't already exist
            // If the table exists, this is skipped
            tx.executeSql('CREATE TABLE IF NOT EXISTS settings(setting TEXT UNIQUE, value TEXT)');
	  });
}

function dropSettings() {
    var db = getDatabase();
    db.transaction(
        function(tx) {
            // Create the settings table if it doesn't already exist
            // If the table exists, this is skipped
            tx.executeSql('DROP TABLE IF EXISTS settings');
	    });
}

// This function is used to write a setting into the database
function setSetting(setting, value) {
    // setting: string representing the setting name (eg: “username”)
    // value: string representing the value of the setting (eg: “myUsername”)
    var db = getDatabase();
    var res = "";
    db.transaction(function(tx) {
        var rs = tx.executeSql('INSERT OR REPLACE INTO settings VALUES (?,?);', [setting, value]);
    });
}

// This function is used to retrieve a setting from the database
function getSetting(setting, defaultValue) {
    console.log('getSetting(setting, defaultValue)',setting, defaultValue)
    var db = getDatabase();
    var res="";
    db.transaction(function(tx) {
        var rs = tx.executeSql('SELECT value FROM settings WHERE setting=?;', [setting]);
        if (rs.rows.length > 0) {
            console.log('fnd', rs.rows.item(0).value)
            res = rs.rows.item(0).value;
        } else {
            console.log('def',defaultValue)
            res = defaultValue;
        }
    });
    return res
}
