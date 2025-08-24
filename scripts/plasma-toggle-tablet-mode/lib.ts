import {$} from 'bun'

const dbus = {
	propGet: 'org.freedesktop.DBus.Properties.Get',
	propSet: 'org.freedesktop.DBus.Properties.Set',
}

export const kwinInputDevice = {
	service: 'org.kde.KWin',
	path: '/org/kde/KWin/InputDevice',
	interface: 'org.kde.KWin.InputDevice',
	deviceMap: new Map<string, string>(),

	async getDevicesSysNames() {
		const dmInterface = 'org.kde.KWin.InputDeviceManager'
		const dmProperty = 'devicesSysNames'
		const response =
			await $`qdbus ${this.service} ${this.path} ${dbus.propGet} ${dmInterface} ${dmProperty}`.text()
		const names = response
			.split('\n')
			.map((line) => line.trim())
			.filter(Boolean)
		return names
	},

	async getDevices(sysName: string) {
		const property = 'name'
		const response =
			await $`qdbus ${this.service} ${this.path}/${sysName} ${dbus.propGet} ${this.interface} ${property}`.text()
		return response.trim()
	},

	async updateDeviceMap() {
		const sysNames = await this.getDevicesSysNames()
		const namePromises = sysNames.map(async (sysName) => {
			const name = await this.getDevices(sysName)
			return {name, sysName}
		})
		const nameSysNamePairs = await Promise.all(namePromises)
		for (const {name, sysName} of nameSysNamePairs) {
			this.deviceMap.set(name, sysName)
		}
	},

	async isDeviceEnabled(sysName: string) {
		const property = 'enabled'
		const response =
			await $`qdbus ${this.service} ${this.path}/${sysName} ${dbus.propGet} ${this.interface} ${property}`.text()
		return response.trim() === 'true'
	},

	async setDeviceStatus(sysName: string, enabled: boolean) {
		const property = 'enabled'
		const value = enabled ? 'true' : 'false'
		await $`qdbus ${this.service} ${this.path}/${sysName} ${dbus.propSet} ${this.interface} ${property} ${value}`
	},
}

export const kconfig = {
	async getTabletMode() {
		const response =
			await $`kreadconfig6 --file=kwinrc --group=Input --key=TabletMode --default=auto`.text()
		return response.trim() === 'on'
	},

	async setTabletMode(value: boolean) {
		const arg = value ? 'on' : 'off'
		await $`kwriteconfig6 --notify --file=kwinrc --group=Input --key=TabletMode ${arg}`
	},
}
