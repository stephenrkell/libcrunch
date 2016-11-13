/*
 * /dev/ones kernel module
 * Stephen Kell <srk31@cl.cam.ac.uk>
 * 
 * Based on "Hello, world!" minimal kernel module - /dev version (hello_dev.c)
 * by Valerie Henson <val@nmt.edu>
 * and
 * linux/drivers/char/mem.c
 * by Linus Torvalds and others.
 *
 */
 
#include <linux/mm.h>
#include <linux/miscdevice.h>
#include <linux/slab.h>
#include <linux/vmalloc.h>
#include <linux/mman.h>
#include <linux/random.h>
#include <linux/init.h>
#include <linux/raw.h>
#include <linux/tty.h>
#include <linux/capability.h>
#include <linux/ptrace.h>
#include <linux/device.h>
#include <linux/highmem.h>
#include <linux/backing-dev.h>
#include <linux/splice.h>
#include <linux/pfn.h>
#include <linux/export.h>
#include <linux/io.h>
#include <linux/uio.h>
#include <linux/aio.h>
#include <linux/pagemap.h>
#include <linux/fs.h>
#include <linux/module.h>
#include <linux/shmem_fs.h>
#include <linux/file.h>

#include <asm/uaccess.h>

#ifdef CONFIG_IA64
# include <linux/efi.h>
#endif

static struct page *ones_page;
static void *ones_data;

static void ones_vma_open(struct vm_area_struct *vma)
{ /*MOD_INC_USE_COUNT;*/ }

static void ones_vma_close(struct vm_area_struct *vma)
{ /*MOD_DEC_USE_COUNT;*/ }

static int ones_vma_fault(struct vm_area_struct *vma, struct vm_fault *vmf)
{
	get_page(ones_page);
	vmf->page = ones_page;
	return 0;
}

static struct vm_operations_struct ones_vm_ops = {
	.open = ones_vma_open,
	.close = ones_vma_close,
	.fault = ones_vma_fault,
};

static int ones_mmap(struct file *file, struct vm_area_struct *vma)
{
#ifndef CONFIG_MMU
	return -ENOSYS;
#endif
	if (vma->vm_flags & VM_SHARED) return -ENOSYS;
	
	//unsigned long offset = VMA_OFFSET(vma);

	//if (offset >= __pa(high_memory) || (filp->f_flags & O_SYNC))
	//	vma->vm_flags |= VM_IO;
	//vma->vm_flags |= VM_RESERVED;

	/* Don't map a page range; use nopage method. */
	//if (remap_page_range(vma->vm_start, offset, vma->vm_end - vma->vm_start,
	//			vma->vm_page_prot))
	//	return -EAGAIN;

	vma->vm_ops = &ones_vm_ops;
	ones_vma_open(vma);
	
	return 0;
}

static unsigned long __setall_user(void __user *addr, unsigned long size)
{
	long __d0;
	might_fault();
	/* no memory constraint because it doesn't change any memory gcc knows
	   about */
#ifdef CONFIG_X86_SMAP
	stac();
#endif
	asm volatile(
			"	   testq  %[size8],%[size8]\n"
			"	   jz	 4f\n"
			"0:	 movq %[ones],(%[dst])\n"
			"	   addq   %[eight],%[dst]\n"
			"	   decl %%ecx ; jnz   0b\n"
			"4:	 movq  %[size1],%%rcx\n"
			"	   testl %%ecx,%%ecx\n"
			"	   jz	 2f\n"
			"1:	 movb   %b[ones],(%[dst])\n"
			"	   incq   %[dst]\n"
			"	   decl %%ecx ; jnz  1b\n"
			"2:\n"
			".section .fixup,\"ax\"\n"
			"3:	 lea 0(%[size1],%[size8],8),%[size8]\n"
			"	   jmp 2b\n"
			".previous\n"
			_ASM_EXTABLE(0b,3b)
			_ASM_EXTABLE(1b,2b)
			: [size8] "=&c"(size), [dst] "=&D" (__d0)
			: [size1] "r"(size & 7), "[size8]" (size / 8), "[dst]"(addr),
			  [ones] "r" ((unsigned long) -1), [eight] "r" (8UL));
#ifdef CONFIG_X86_SMAP
	clac();
#endif
	return size;
}

/*
 * Special lseek() function for /dev/ones.  Most notably, you
 * can fopen() both devices with "a" now.  This was previously impossible.
 * -- SRB.
 */
static loff_t ones_lseek(struct file *file, loff_t offset, int orig)
{
	return file->f_pos = 0;
}

static ssize_t ones_write(struct file *file, const char __user *buf,
			  size_t count, loff_t *ppos)
{
	return count;
}

static ssize_t ones_read(struct file *file, char __user *buf,
			 size_t count, loff_t *ppos)
{
	size_t written;

	if (!count)
		return 0;

	if (!access_ok(VERIFY_WRITE, buf, count))
		return -EFAULT;

	written = 0;
	while (count) {
		unsigned long unwritten;
		size_t chunk = count;

		if (chunk > PAGE_SIZE)
			chunk = PAGE_SIZE;	/* Just for latency reasons */
		unwritten = __setall_user(buf, chunk);
		written += chunk - unwritten;
		if (unwritten)
			break;
		if (signal_pending(current))
			return written ? written : -ERESTARTSYS;
		buf += chunk;
		count -= chunk;
		cond_resched();
	}
	return written ? written : -EFAULT;
}


static const struct file_operations ones_fops = {
	.llseek		= ones_lseek,
	.read		= ones_read,
	.write		= ones_write,
	//.read_iter	= read_iter_ones,
	//.aio_write	= aio_write_ones,
	.mmap		= ones_mmap,
};
/*
 * capabilities for /dev/ones
 * - permits private mappings, "copies" are taken of the source of zeros
 * - no writeback happens
 */
// FIXME: this is unused
//static struct backing_dev_info ones_bdi = {
//	.name		= "char/mem",
//	.capabilities	= BDI_CAP_MAP_COPY | BDI_CAP_NO_ACCT_AND_WRITEBACK,
//};

static struct miscdevice ones_dev = {
	/*
	 * We don't care what minor number we end up with, so tell the
	 * kernel to just pick one.
	 */
	.minor = MISC_DYNAMIC_MINOR,
	/*
	 * Name ourselves /dev/ones.
	 */
	.name = "ones",
	.mode = 0666,
	/*
	 * What functions to call when a program performs file
	 * operations on the device.
	 */
	.fops = &ones_fops
};

// static struct class *memmod_class;

static int __init ones_init(void)
{
	// int minor;
	int err;
	void *end;
	unsigned long *l;

	//err = bdi_init(&ones_bdi);
	//if (err)
	//	return err;

	//if (register_chrdev(MEMMOD_MAJOR, "mem", &memory_fops))
	//	printk("unable to get major %d for memory devs\n", MEMMOD_MAJOR);

// 	memmod_class = class_create(THIS_MODULE, "memmod");
// 	if (IS_ERR(memmod_class))
// 		return PTR_ERR(memmod_class);
// 
// 	memmod_class->devnode = mem_devnode;
// 	
// 	device_create(memmod_class, NULL, MKDEV(MEMMOD_MAJOR, minor),
// 			      NULL, "ones");

	/* Get a fresh page to be our "ones" page, and initialize it to all-ones. */
	ones_data = (char*) __get_free_page(GFP_KERNEL);
	ones_page = virt_to_page(ones_data);
	end = (char*) ones_data + PAGE_SIZE;
	for (l = ones_data; l != end; ++l) *l = (unsigned long) -1;

	/* Create the "ones" device in the /sys/class/mem directory.
	 * Udev will automatically create the /dev/ones device using
	 * the default rules.
	 */
	err = misc_register(&ones_dev);
	if (err) printk(KERN_ERR "Unable to register /dev/ones mem device\n");

	return err;
	// return tty_init();
}

module_init(ones_init);

static void __exit
ones_exit(void)
{
	misc_deregister(&ones_dev);
	free_page((unsigned long) ones_data);
}

module_exit(ones_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Stephen Kell <srk31@cl.cam.ac.uk>");
MODULE_DESCRIPTION("/dev/ones module");
MODULE_VERSION("dev");
