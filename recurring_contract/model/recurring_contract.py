# -*- encoding: utf-8 -*-
##############################################################################
#
#    Copyright (C) 2014 Compassion CH (http://www.compassion.ch)
#    Releasing children from poverty in Jesus' name
#    @author: Cyril Sester <csester@compassion.ch>
#
#    The licence is in the file __openerp__.py
#
##############################################################################

from datetime import datetime
from dateutil.relativedelta import relativedelta

import openerp
from openerp.osv import orm
from openerp import api, exceptions, fields, models, netsvc
from openerp.tools import DEFAULT_SERVER_DATE_FORMAT as DF
from openerp.tools.translate import _
import openerp.addons.decimal_precision as dp


class recurring_contract_line(models.Model):
    """ Each product sold through a contract """

    _name = "recurring.contract.line"
    _description = "A contract line"

    def name_get(self, ids):
        if not ids:
            return []
        res = [(cl.id, cl.product_id.name_template) for cl in self.browse(ids)]
        return res

    contract_id = openerp.fields.Many2one(
        'recurring.contract', _('Contract'), required=True,
        ondelete='cascade', readonly=True)
    product_id = openerp.fields.Many2one('product.product', _('Product'),
                                         required=True)
    amount = openerp.fields.Float(_('Price'), required=True)
    quantity = openerp.fields.Integer(_('Quantity'), default=1, required=True)
    # 'subtotal': fields.function(
            # _compute_subtotal, string='Subtotal', type="float",
            # digits_compute=dp.get_precision('Account'), store={
                # 'recurring.contract.line': (
                    # lambda self, cr, uid, ids, c=None: ids,
                    # ['amount', 'quantity'], 10)
            # }),
    subtotal = openerp.fields.Float(compute='_compute_subtotal')

    @api.one
    def _compute_subtotal(self):
        self.subtotal = self.amount * self.quantity

    @api.onchange('product_id')
    def on_change_product_id(self):
        if not self.product_id:
            self.amount = 0.0
        else:
            self.amount = self.product_id.list_price


class recurring_contract(models.Model):
    """ A contract to perform recurring invoicing to a partner """

    _name = "recurring.contract"
    _description = "Contract for recurring invoicing"
    _inherit = ['mail.thread']
    _rec_name = 'reference'

    @api.one
    def _get_total_amount(self):
        self.total_amount = sum([line.subtotal for line in
                                 self.contract_line_ids])

    # def _get_contract_from_group(group_obj, cr, uid, group_ids, context=None):
        # self = group_obj.pool.get('recurring.contract')
        # return self.search(cr, uid, [('group_id', 'in', group_ids)],
                           # context=context)

    # def _get_contract_from_line(self, cr, uid, ids, context=None):
        # contract_ids = []
        # contract_line_obj = self.pool.get('recurring.contract.line')
        # for contract_line in contract_line_obj.browse(cr, uid, ids, context):
            # contract_ids.append(contract_line.contract_id.id)
        # return contract_ids
    @api.one
    def _get_last_paid_invoice(self):
        self.last_paid_invoice_date = max(
            [invl.invoice_id.date_invoice for invl in self.invoice_line_ids
             if invl.state == 'paid'] or [False])

    reference = fields.Char(
        _('Reference'), default="/", required=True, readonly=True,
        states={'draft': [('readonly', False)]})
    start_date = fields.Date(
        _('Start date'), default=datetime.today().strftime(DF),
        required=True, readonly=True,
        states={'draft': [('readonly', False)]},
        track_visibility="onchange")
    end_date = fields.Date(
        _('End date'), readonly=False,
        states={'terminated': [('readonly', True)]},
        track_visibility="onchange")
    next_invoice_date = fields.Date(
        _('Next invoice date'), readonly=False,
        states={'draft': [('readonly', False)]},
        track_visibility="onchange")
    last_paid_invoice_date = fields.Date(
        compute='_get_last_paid_invoice', string=_('Last paid invoice date'))
    partner_id = fields.Many2one(
        'res.partner', string=_('Partner'), required=True,
        readonly=True, states={'draft': [('readonly', False)]},
        ondelete='restrict')
    group_id = fields.Many2one(
        'recurring.contract.group', _('Payment Options'),
        required=True, ondelete='cascade',
        track_visibility="onchange")
    invoice_line_ids = fields.One2many(
        'account.invoice.line', 'contract_id',
        _('Related invoice lines'), readonly=True)
    contract_line_ids = fields.One2many(
        'recurring.contract.line', 'contract_id',
        _('Contract lines'), track_visibility="onchange")
    state = fields.Selection([
        ('draft', _('Draft')),
        ('active', _('Active')),
        ('terminated', _('Terminated'))], _('Status'), default='draft',
        select=True, readonly=True, track_visibility='onchange',
        help=_(" * The 'Draft' status is used when a user is encoding a "
               "new and unconfirmed Contract.\n"
               "* The 'Active' status is used when the contract is "
               "confirmed and until it's terminated.\n"
               "* The 'Terminated' status is used when a contract is no "
               "longer active."))
    # 'total_amount': fields.function(
            # _get_total_amount, string='Total',
            # digits_compute=dp.get_precision('Account'),
            # store={
                # 'recurring.contract': (lambda self, cr, uid, ids, c=dict():
                                       # ids, ['contract_line_ids'], 40),
                # 'recurring.contract.line': (_get_contract_from_line,
                                            # ['amount', 'quantity'], 30),
            # }, track_visibility="onchange"),
    total_amount = fields.Float(
        compute='_get_total_amount', string='Total',
        digits_compute=dp.get_precision('Account'),
        track_visibility="onchange")
            # 'payment_term_id': fields.related(
            # 'group_id', 'payment_term_id', relation='account.payment.term',
            # type="many2one", readonly=True, string=_('Payment Term'),
            # store={
                # 'recurring.contract.group': (
                    # _get_contract_from_group,
                    # ['payment_term_id'], 10)}),
    payment_term_id = fields.Many2one(
        'account.payment.term', readonly=True, string=_('Payment Term'),
        store=True)

    @api.constrains('reference')
    @api.one
    def _check_unique_reference(self):
        chk_list_contracts = self.search([]) - self
        ref_lst = [contract.reference for contract in chk_list_contracts
                   if contract.reference]
        if self.reference in ref_lst:
            raise exceptions.ValidationError(
                _('Error: Reference should be unique'))
        return True

    #################################
    #        PUBLIC METHODS         #
    #################################
    def create(self, cr, uid, vals, context=None):
        """ Add a sequence generated ref if none is given """
        if vals.get('reference', '/') == '/':
            vals['reference'] = self.pool.get('ir.sequence').next_by_code(
                cr, uid, 'recurring.contract.ref', context=context)
        return super(recurring_contract, self).create(cr, uid, vals,
                                                      context=context)

    @api.one
    def write(self, vals):
        """ Perform various checks when a contract is modified. """
        if 'next_invoice_date' in vals:
            self._on_change_next_invoice_date(vals['next_invoice_date'])

        res = super(recurring_contract, self).write(vals)

        res = super(recurring_contract, self).write(
            cr, uid, ids, vals, context=context)

        if 'contract_line_ids' in vals:
            self._on_contract_lines_changed()

        return res

    def copy(self, cr, uid, id, default=None, context=None):
        default = default or dict()
        today = datetime.today()
        old_contract = self.browse(cr, uid, id, context)
        next_invoice_date = datetime.strptime(old_contract.next_invoice_date,
                                              DF)
        next_invoice_date = next_invoice_date.replace(month=today.month)
        default.update({
            'state': 'draft',
            'reference': '/',
            'start_date': today.strftime(DF),
            'end_date': False,
            'next_invoice_date': next_invoice_date.strftime(DF),
            'invoice_line_ids': False,
        })
        return super(recurring_contract, self).copy(cr, uid, id, default,
                                                    context)

    @api.one
    def unlink(self):
        if self.state not in ('draft', 'terminated'):
            raise exceptions.Warning(
                'UserError',
                _('You cannot delete a contract that is still active. '
                  'Terminate it first.'))
        else:
            super(recurring_contract, self).unlink()

        return True

    def button_generate_invoices(self, cr, uid, ids, context=None):
        group_ids = [contract.group_id.id for contract in self.browse(
            cr, uid, ids, context)]
        contract_group_obj = self.pool.get('recurring.contract.group')
        return contract_group_obj.button_generate_invoices(
            cr, uid, group_ids, context)

    @api.one
    def clean_invoices(self, context=None, since_date=None,
                       to_date=None, keep_lines=None):
        """ This method deletes invoices lines generated for a given contract
            having a due date >= current month. If the invoice_line was the
            only line in the invoice, we cancel the invoice. In the other
            case, we have to revalidate the invoice to update the move lines.
        """
        invl_search = [('contract_id', '=', self.id),
                       ('state', 'not in', ('paid', 'cancel'))]
        if since_date:
            invl_search.append(('due_date', '>=', since_date))
        if to_date:
            invl_search.append(('due_date', '<=', to_date))
        inv_line_obj = self.env['account.invoice.line']

        # Find all unpaid invoice lines after the given date
        inv_line_ids = self.invoice_line_ids.filtered(
            lambda record: record.state not in ('paid', 'cancel'))

        inv_ids = set()
        empty_inv_ids = set()
        to_remove_ids = []   # Invoice lines that will be moved or removed
        for inv_line in inv_line_ids:
            invoice = inv_line.invoice_id
            inv_ids.add(invoice.id)
            # Check if invoice is empty after removing the invoice_lines
            # of the given contract
            if invoice.id not in empty_inv_ids:
                remaining_lines_ids = [
                    invl.id for invl in invoice.invoice_line if
                    not invl.contract_id or
                    invl.contract_id and invl.contract_id.id != self.id]
                if remaining_lines_ids:
                    # We can move or remove the line
                    to_remove_ids.append(inv_line.id)
                else:
                    # The invoice would be empty if we remove the line
                    empty_inv_ids.add(invoice.id)

        if keep_lines:
            self._move_cancel_lines(to_remove_ids, keep_lines)
        else:
            to_remove_recset = inv_line_obj.browse(to_remove_ids)
            to_remove_recset.unlink()

        # Invoices to set back in open state
        renew_inv_ids = list(inv_ids - empty_inv_ids)

        self._cancel_confirm_invoices(list(inv_ids), renew_inv_ids,
                                      keep_lines)

        return inv_ids

    @api.one
    def _cancel_confirm_invoices(self, cancel_ids, confirm_ids,
                                 keep_lines=None):
        """ Cancels given invoices and validate again given invoices.
            confirm_ids must be a subset of cancel_ids ! """
        inv_obj = self.pool.get('account.invoice')
        wf_service = netsvc.LocalService('workflow')
        for invoice_id in cancel_ids:
            wf_service.trg_validate(self.env.user.id, 'account.invoice',
                                    invoice_id, 'invoice_cancel', self.env.cr)
        inv_obj.action_cancel_draft(self.env.cr, self.env.user.id,
                                    confirm_ids)
        for invoice_id in confirm_ids:
            wf_service.trg_validate(self.env.user.id, 'account.invoice',
                                    invoice_id, 'invoice_open', self.env.cr)

    def rewind_next_invoice_date(self, cr, uid, ids, context):
        """ Rewinds the next invoice date of contract after the last
        generated invoice. No open invoices exist after that date. """
        gen_states = self.pool.get(
            'recurring.contract.group')._get_gen_states()
        for contract in self.browse(cr, uid, ids, context):
            if contract.state in gen_states:
                last_invoice_date = max([
                    datetime.strptime(line.invoice_id.date_invoice, DF) for
                    line in contract.invoice_line_ids
                    if line.state in ('open', 'paid')] or [False])
                if last_invoice_date:
                    # Call super for allowing rewind.
                    super(recurring_contract, self).write(
                        cr, uid, [contract.id], {
                            'next_invoice_date':
                            last_invoice_date.strftime(DF)}, context)
                    contract.update_next_invoice_date()
                else:
                    # No open/paid invoices, look for cancelled ones
                    next_invoice_date = min([
                        datetime.strptime(line.invoice_id.date_invoice, DF)
                        for line in contract.invoice_line_ids
                        if line.state == 'cancel'])
                    if next_invoice_date:
                        super(recurring_contract, self).write(
                            cr, uid, [contract.id], {
                                'next_invoice_date':
                                next_invoice_date.strftime(DF)}, context)

        return True

    #################################
    #        PRIVATE METHODS        #
    #################################
    def update_next_invoice_date(self, cr, uid, ids, context=None):
        """ Recompute and set next_invoice date. """
        for contract in self.browse(cr, uid, ids, context):
            next_date = self._compute_next_invoice_date(contract)
            contract.write({'next_invoice_date': next_date})
        return True

    def _compute_next_invoice_date(self, contract):
        """ Compute next_invoice_date for a single contract. """
        next_date = datetime.strptime(contract.next_invoice_date, DF)
        rec_unit = contract.group_id.recurring_unit
        rec_value = contract.group_id.recurring_value
        if rec_unit == 'day':
            next_date = next_date + relativedelta(days=+rec_value)
        elif rec_unit == 'week':
            next_date = next_date + relativedelta(weeks=+rec_value)
        elif rec_unit == 'month':
            next_date = next_date + relativedelta(months=+rec_value)
        else:
            next_date = next_date + relativedelta(years=+rec_value)

        return next_date.strftime(DF)

    @api.one
    def _update_invoice_lines(self, invoice_ids):
        """Update invoice lines generated by a contract, when the contract
        was modified and corresponding invoices were cancelled.

        Parameters:
            - invoice_ids (list): ids of draft invoices to be
                                  updated and validated
        """
        invoice_obj = self.env['account.invoice']
        inv_line_obj = self.env['account.invoice.line']
        group_obj = self.env['recurring.contract.group']
        for invoice in invoice_obj.browse(invoice_ids):
            # Update payment term and generate new invoice_lines
            invoice.write({
                'payment_term': self.group_id.payment_term_id and
                self.group_id.payment_term_id.id or False})
            old_lines_ids = [invl.id for invl in invoice.invoice_line
                             if invl.contract_id.id == self.id]
            inv_line_obj.browse(old_lines_ids).unlink()
            self.with_context(no_next_date_update=True)
            group_obj._generate_invoice_lines(self, invoice.id)
            self.with_context(no_next_date_update=False)

    @api.one
    def _on_change_next_invoice_date(self, new_invoice_date):
        new_invoice_date = datetime.strptime(new_invoice_date, DF)
        if self.next_invoice_date:
            next_invoice_date = datetime.strptime(self.next_invoice_date, DF)
            if next_invoice_date > new_invoice_date:
                raise orm.except_orm(
                    'Error', _('You cannot rewind the next invoice date.'))
        return True

    def _on_contract_lines_changed(self):
        """Update related invoices to reflect the changes to the contract.
        """
        invoice_obj = self.env['account.invoice']
        inv_line_obj = self.env['account.invoice.line']
        # Find all unpaid invoice lines after the given date
        since_date = datetime.today().replace(day=1).strftime(DF)
        inv_line_ids = inv_line_obj.search(
            [('contract_id', '=', self.id),
             ('due_date', '>=', since_date),
             ('state', 'not in', ('paid', 'cancel'))])

        con_ids = set()
        inv_ids = set()
        for inv_line in inv_line_ids:
            invoice = inv_line.invoice_id
            if invoice.id not in inv_ids or \
                    inv_line.contract_id.id not in con_ids:
                con_ids.add(inv_line.contract_id.id)
                inv_ids.add(invoice.id)
                invoice.action_cancel()
                invoice.action_cancel_draft()
                self._update_invoice_lines([invoice.id])

        wf_service = netsvc.LocalService('workflow')
        for invoice in invoice_obj.browse(list(inv_ids)):
            wf_service.trg_validate(self.env.user.id, 'account.invoice',
                                    invoice.id, 'invoice_open', self.env.cr)

    @api.one
    def _move_cancel_lines(self, invoice_line_ids, message=None):
        """ Method that takes out given invoice_lines from their invoice
        and put them in a cancelled copy of that invoice.
        Warning : this method does not recompute totals of original invoices,
                  and does not update related move lines.
        """
        invoice_obj = self.env['account.invoice']
        invoice_line_obj = self.env['account.invoice.line']
        invoices_copy = dict()
        for invoice_line in invoice_line_obj.browse(invoice_line_ids):
            invoice = invoice_line.invoice_id
            copy_invoice_id = invoices_copy.get(invoice.id)
            if not copy_invoice_id:
                invoice_obj.copy(invoice.id, {
                    'date_invoice': invoice.date_invoice})
                copy_invoice_id = invoice_obj.search(
                    [('partner_id', '=', invoice.partner_id.id),
                     ('state', '=', 'draft'), ('id', '!=', invoice.id),
                     ('date_invoice', '=', invoice.date_invoice)])[0]
                # Empty the new invoice
                cancel_lines = invoice_line_obj.search([
                    ('invoice_id', '=', copy_invoice_id)])
                invoice_line_obj.unlink(cancel_lines)
                invoices_copy[invoice.id] = copy_invoice_id

            # Move the line in the invoice copy
            invoice_line.write({'invoice_id': copy_invoice_id})

        # Compute and cancel invoice copies
        cancel_ids = invoices_copy.values()
        if cancel_ids:
            invoice_obj.button_compute(cancel_ids, set_total=True)
            wf_service = netsvc.LocalService('workflow')
            for cancel_id in cancel_ids:
                wf_service.trg_validate(
                    self.env.user.id, 'account.invoice', cancel_id,
                    'invoice_cancel', self.env.cr)

            self.env.with_context(thread_model='account.invoice')
            self.pool.get('mail.thread').message_post(
                message, _("Invoice Cancelled"), 'comment')

        return True

    ##########################
    #        CALLBACKS       #
    ##########################
    @api.onchange('start_date')
    def on_change_start_date(self):
        """ We automatically update next_invoice_date on start_date change """
        if self.start_date:
            self.next_invoice_date = self.start_date
        return

    @api.onchange('partner_id')
    def on_change_partner_id(self):
        """ On partner change, we update the group_id. If partner has
        only 1 group, we take it. Else, we take nothing.
        """
        group_ids = self.env['recurring.contract.group'].search(
            [('partner_id', '=', self.partner_id.id)])

        self.group_id = None
        if len(group_ids) == 1:
            self.group_id = group_ids[0]
        return

    @api.one
    def contract_draft(self):
        self.state = 'draft'
        return True

    @api.one
    def contract_active(self):
        self.state = 'active'
        return True

    def contract_terminated(self, cr, uid, ids, context=None):
        today = datetime.today().strftime(DF)
        self.write(cr, uid, ids, {'state': 'terminated', 'end_date': today})
        self.clean_invoices(cr, uid, ids, context)
        return True

    def end_date_reached(self, cr, uid, context=None):
        today = datetime.today().strftime(DF)
        contract_ids = self.search(cr, uid, [('state', '=', 'active'),
                                             ('end_date', '<=', today)],
                                   context=context)

        if contract_ids:
            self.contract_terminated(cr, uid, contract_ids, context=context)

        return True
