/* See LICENSE file for copyright and license details. */
#include <stdio.h>

#include "../slstatus.h"
#include "../util.h"

#if defined(__linux__)
	#include <stdint.h>

	const char *
	ram_free(const char *unused)
	{
		uintmax_t free;
		FILE *fp;

		if (!(fp = fopen("/proc/meminfo", "r")))
			return NULL;

		if (lscanf(fp, "MemFree:", "%ju kB", &free) != 1) {
			fclose(fp);
			return NULL;
		}

		fclose(fp);
		return fmt_human(free * 1024, 1024);
	}

	const char *
	ram_perc(const char *unused)
	{
		uintmax_t total, free, buffers, cached, shmem, sreclaimable;
		int percent;
		FILE *fp;

		if (!(fp = fopen("/proc/meminfo", "r")))
			return NULL;

		if (lscanf(fp, "MemTotal:", "%ju kB", &total)  != 1 ||
		    lscanf(fp, "MemFree:", "%ju kB", &free)    != 1 ||
		    lscanf(fp, "Buffers:", "%ju kB", &buffers) != 1 ||
		    lscanf(fp, "Cached:", "%ju kB", &cached)   != 1 ||
		    lscanf(fp, "Shmem:", "%ju kB", &shmem)     != 1 ||
		    lscanf(fp, "SReclaimable:", "%ju kB", &sreclaimable) != 1) {
			fclose(fp);
			return NULL;
		}
		fclose(fp);

		if (total == 0)
			return NULL;

		percent = 100 * (total - free - buffers - cached - sreclaimable + shmem) / total;
		return bprintf("%d", percent);
	}

	const char *
	ram_total(const char *unused)
	{
		uintmax_t total;

		if (pscanf("/proc/meminfo", "MemTotal: %ju kB\n", &total)
		    != 1)
			return NULL;

		return fmt_human(total * 1024, 1024);
	}

	const char *
	ram_used(const char *unused)
	{
		uintmax_t total, free, buffers, cached, used, shmem, sreclaimable;
		FILE *fp;

		if (!(fp = fopen("/proc/meminfo", "r")))
			return NULL;

		if (lscanf(fp, "MemTotal:", "%ju kB", &total)  != 1 ||
		    lscanf(fp, "MemFree:", "%ju kB", &free)    != 1 ||
		    lscanf(fp, "Buffers:", "%ju kB", &buffers) != 1 ||
		    lscanf(fp, "Cached:", "%ju kB", &cached)   != 1 ||
		    lscanf(fp, "Shmem:", "%ju kB", &shmem)     != 1 ||
		    lscanf(fp, "SReclaimable:", "%ju kB", &sreclaimable) != 1) {
			fclose(fp);
			return NULL;
		}
		fclose(fp);

		used = total - free - buffers - cached - sreclaimable + shmem;
		return fmt_human(used * 1024, 1024);
	}
#elif defined(__OpenBSD__)
	#include <stdlib.h>
	#include <sys/sysctl.h>
	#include <sys/types.h>
	#include <unistd.h>

	#define LOG1024 10
	#define pagetok(size, pageshift) (size_t)(size << (pageshift - LOG1024))

	inline int
	load_uvmexp(struct uvmexp *uvmexp)
	{
		int uvmexp_mib[] = {CTL_VM, VM_UVMEXP};
		size_t size;

		size = sizeof(*uvmexp);

		if (sysctl(uvmexp_mib, 2, uvmexp, &size, NULL, 0) >= 0)
			return 1;

		return 0;
	}

	const char *
	ram_free(const char *unused)
	{
		struct uvmexp uvmexp;
		int free_pages;

		if (!load_uvmexp(&uvmexp))
			return NULL;

		free_pages = uvmexp.npages - uvmexp.active;
		return fmt_human(pagetok(free_pages, uvmexp.pageshift) *
				 1024, 1024);
	}

	const char *
	ram_perc(const char *unused)
	{
		struct uvmexp uvmexp;
		int percent;

		if (!load_uvmexp(&uvmexp))
			return NULL;

		percent = uvmexp.active * 100 / uvmexp.npages;
		return bprintf("%d", percent);
	}

	const char *
	ram_total(const char *unused)
	{
		struct uvmexp uvmexp;

		if (!load_uvmexp(&uvmexp))
			return NULL;

		return fmt_human(pagetok(uvmexp.npages,
					 uvmexp.pageshift) * 1024, 1024);
	}

	const char *
	ram_used(const char *unused)
	{
		struct uvmexp uvmexp;

		if (!load_uvmexp(&uvmexp))
			return NULL;

		return fmt_human(pagetok(uvmexp.active,
					 uvmexp.pageshift) * 1024, 1024);
	}
#elif defined(__FreeBSD__)
	#include <sys/sysctl.h>
	#include <sys/vmmeter.h>
	#include <unistd.h>
	#include <vm/vm_param.h>

	const char *
	ram_free(const char *unused) {
		struct vmtotal vm_stats;
		int mib[] = {CTL_VM, VM_TOTAL};
		size_t len;

		len = sizeof(struct vmtotal);
		if (sysctl(mib, 2, &vm_stats, &len, NULL, 0) < 0
		    || !len)
			return NULL;

		return fmt_human(vm_stats.t_free * getpagesize(), 1024);
	}

	const char *
	ram_total(const char *unused) {
		unsigned int npages;
		size_t len;

		len = sizeof(npages);
		if (sysctlbyname("vm.stats.vm.v_page_count",
		                 &npages, &len, NULL, 0) < 0 || !len)
			return NULL;

		return fmt_human(npages * getpagesize(), 1024);
	}

	const char *
	ram_perc(const char *unused) {
		unsigned int npages;
		unsigned int active;
		size_t len;

		len = sizeof(npages);
		if (sysctlbyname("vm.stats.vm.v_page_count",
		                 &npages, &len, NULL, 0) < 0 || !len)
			return NULL;

		if (sysctlbyname("vm.stats.vm.v_active_count",
		                 &active, &len, NULL, 0) < 0 || !len)
			return NULL;

		return bprintf("%d", active * 100 / npages);
	}

	const char *
	ram_used(const char *unused) {
		unsigned int active;
		size_t len;

		len = sizeof(active);
		if (sysctlbyname("vm.stats.vm.v_active_count",
		                 &active, &len, NULL, 0) < 0 || !len)
			return NULL;

		return fmt_human(active * getpagesize(), 1024);
	}
#endif
